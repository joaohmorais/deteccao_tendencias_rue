# SCRIPT DE CLASSIFICAÇÃO DOS ALERTAS RUE
# CIE-SMS Rio
# Última atualização: 11/09/2023

library(tidyverse)
library(lubridate)
library(duckdb)

# have to specify dir due to cronR
path_dir <- "/Data/dadoscoe/Desenvolvimento/Shiny_COVID/beast_alerts/"

## Paths ----

paths <- read.csv(paste0("paths.csv"))
db_path <- paths$db_path[1]
aux_fct_path <- paths$aux_functions_locale[1]

source(paste0(aux_fct_path, "aux_functions.R"))
source(paste0(aux_fct_path, "beast_aux_functions.R"))

DUCK_DB_RUE <- paste0(db_path, "dados_rue.duckdb") # path do banco
DUCK_DB_CID <- paste0(db_path, "cids.duckdb") # path do banco
DUCK_DB_ALERTAS <- paste0(db_path, "alertas_rue.duckdb")

# ALERT PARAMETERS ----

moving_series <- c("covid")
denom_var <- "tot_atend" # other options: "z00"
patamar_var <- "gam" # other options: "n", "trend"

# BEAST calculations ----

con_alt <- dbConnect(
  duckdb(),
  DUCK_DB_ALERTAS,
  read_only = TRUE
)

beast_df <- con_alt %>% 
  dbGetQuery(
    "select a.DATA, 
          a.serie,
          round(a.Y_obs) AS n,
          a.trend_est AS trend,
          a.slope_est AS slope,
          a.prob_pos_slope AS prob_pos, 
          a.prob_zero_slope AS prob_zero,
          a.prob_neg_slope AS prob_neg,
          a.prob_changepoint,
          a.is_probable_changepoint
          from tb_beast as a
          right join (
            select serie,
            max(fitted_date) AS fitted_date
            from tb_beast
            group by serie
          ) as b on a.serie = b.serie
            and a.fitted_date = b.fitted_date
            where a.nivel = 'MUNICIPIO'
            order by DATA asc, a.serie asc"
  ) %>% as_tibble() %>% 
  left_join(
    get_total_attendances(), by = c("DATA")
  ) %>% 
  left_join(
    get_z00_attendances(), by = c("DATA")
  )

beast_df <- beast_df %>% 
  nest(.by = serie) %>% 
  mutate(smoothed_slope = map(data, get_beast_smoothed_slope)) %>% 
  mutate(alerts = map(smoothed_slope, get_smoothed_slope_alerts)) %>% 
  mutate(
    # Calculate days since alert change
    last_change = map_int(alerts, 
                          function(df) {
                            if(!is.null(df)) {
                              last_date <- max(df$DATA, na.rm=TRUE)
                              current <- df$alert[df$DATA == last_date]
                              sub_df <- df %>% filter(alert != current)
                              last_change_date <- max(sub_df$DATA, na.rm=TRUE)
                              
                              return(
                                difftime(last_date, 
                                         last_change_date, 
                                         units = "days") %>% 
                                  as.numeric() %>% 
                                  as.integer()
                              )  
                            }
                            return(-1)
                          }
    ),
    # Last changepoint
    last_changepoint = map_vec(data, 
                               function(beast_df) {
                                 sub_df <- beast_df %>% 
                                   filter(is_probable_changepoint)
                                 
                                 if(nrow(sub_df) > 0) {
                                   return(
                                     max(sub_df$DATA, na.rm=TRUE)
                                   )
                                 }
                                 
                                 return(-Inf)
                               }
    ))

beast_df <- beast_df %>% 
  select(serie, data) %>% 
  unnest(cols = c(data)) %>% 
  left_join(beast_df %>% 
              select(serie, alerts, 
                     last_change, last_changepoint) %>% 
              unnest(cols = c(alerts))
  )

smooth_df <- con_alt %>% 
  dbGetQuery("select a.DATA,
              a.serie as serie,
              a.smooth as smoothed_n
             from tb_fitted as a
             right join (
              select serie, 
              max(fitted_date) as fitted_date
              from tb_fitted
              group by serie
             ) as b on a.serie = b.serie and
                 a.fitted_date = b.fitted_date
                 where nivel like 'MUNICIPIO'
                 order by DATA asc, a.serie asc") %>% 
  as_tibble()

beast_df <- beast_df %>% 
  left_join(smooth_df, 
            by = c("DATA", "serie"))

beast_df <- beast_df %>% 
  mutate(moving = serie %in% moving_series)

if (denom_var == "tot_atend") {
 beast_df <- beast_df %>% 
   mutate(denom = n_tot)
} else if (denom_var == "z00") {
  beast_df <- beast_df %>% 
    mutate(denom = n_z00)
}

beast_df <- beast_df %>% 
  group_by(serie) %>% 
  mutate(
    razao = smoothed_n/denom,
    razao_med = calculate_moving_metric(
      razao, 
      metric = "mean", 
      moving = unique(moving), 
      window = 180
    ),
    razao_q2 = calculate_moving_metric(
      razao, 
      metric = "quantile",
      probs = 0.2,
      moving = unique(moving),
      window = 180
    ),
    razao_median = calculate_moving_metric(
      razao, 
      metric = "quantile",
      probs = 0.5,
      moving = unique(moving),
      window = 180),
    razao_q8 = calculate_moving_metric(
      razao, 
      metric = "quantile",
      probs = 0.8,
      moving = unique(moving),
      window = 180),
    denom_loess = fitted(loess(denom ~ as.numeric(DATA), span = 0.8)),
    inf = denom_loess*razao_q2,
    med = denom_loess*razao_median,
    sup = pmax(1, denom_loess*razao_q8)
  ) %>% 
  ungroup()

if (patamar_var == "gam") {
  beast_df <- beast_df %>% 
    mutate(patamar_var = smoothed_n)
} else if (patamar_var == "n") {
  beast_df <- beast_df %>% 
    mutate(patamar_var = n)
} else if (patamar_var == "trend") {
  beast_df <- beast_df %>% 
    mutate(patamar_var = trend)
}

beast_df <- beast_df %>% 
  mutate(patamar = case_when(
    patamar_var > sup ~ "acima",
    patamar_var < inf ~ "abaixo",
    TRUE ~ "normal"
  ))

con_alt %>% dbDisconnect(shutdown=TRUE)

beast_latest_alert <- 
  beast_df %>% 
  filter(DATA == max(DATA)) %>% 
  select(DATA, serie, n, slope_smooth, alert, last_change, last_changepoint, patamar) %>% 
  mutate(denom = denom_var, patamar_var = patamar_var)

con_alt <- dbConnect(
  duckdb(),
  DUCK_DB_ALERTAS,
  read_only = FALSE
)

max_date <- unique(beast_latest_alert$DATA)

if ("tb_alert" %in% dbListTables(con_alt)) {
  dbExecute(
    con_alt,
    paste0(
      "delete from tb_alert where DATA = '",
      max_date,
      "';")
  ) 
}

dbWriteTable(
  con_alt,
  "tb_alert", 
  beast_latest_alert,
  append=TRUE
)

dbDisconnect(con_alt, shutdown=TRUE)

gc()