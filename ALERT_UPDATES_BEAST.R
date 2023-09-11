# SCRIPT DE ATUALIZAÇÃO DE SÉRIES SUAVIZADAS E ALERTAS RUE
# CIE-SMS Rio
# Última atualização: 11/09/2023

# 0. SETUP ----

library(tidyverse)
library(lubridate)

aweek::set_week_start("Sunday")

library(duckdb)

# GAM simulations
N_SIMS <- 100

# log function
log_msg <- function(msg, tag=NULL) {
  print(
    paste0(
      "[",
      ifelse(is.null(tag), "", paste0(tag, "-")),
      format(Sys.time(), "%H:%M:%S"), "] ",
      msg
    )
  )
}

log_msg("Iniciando atualização...")

## Paths ----

paths <- read.csv("paths.csv")
beast_params <- read.csv("beast_params.csv")
global_params <- read.csv("global_params.csv")
db_path <- paths$db_path[1]

DUCK_DB_RUE <- paste0(db_path, "dados_rue.duckdb") # path do banco
DUCK_DB_ALERTAS <- paste0(db_path, "alertas_rue.duckdb")

## Aux functions ----

source("aux_deteccao_tendencias_rue.R")
source("tidy_beast.R")
source("fit_beast.R")

# 1. SERIES RETRIEVAL ----

log_msg("Recuperando séries RUE....")

con_rue <- dbConnect(duckdb::duckdb(), DUCK_DB_RUE, read_only=TRUE)

## Municipality ----

series_mun <- dbGetQuery(con_rue, "select * from vw_series_dia;") %>% 
  as_tibble() %>% 
  mutate(nivel = "MUNICIPIO", unidade_nome = NA) %>% 
  relocate(n, .after = everything())

## Units ----

series_units <- dbGetQuery(con_rue, 
                           "select
                        		data_entrada,
                        		semana_epi,
                        		ano_epi,
                        		unidade_nome,
                        		serie,
                        		sum(n) as n
                        	from tb_series
                        	group by data_entrada, semana_epi, ano_epi, unidade_nome, serie 
                        	order by data_entrada asc, serie asc, unidade_nome asc") %>% 
  as_tibble() %>% 
  mutate(nivel = "UNIDADE") 
dbDisconnect(con_rue, shutdown=TRUE)

## Nested ----

log_msg("Aninhando séries...")

series_nest <- 
  series_mun %>% 
  add_row(series_units) %>% 
  rename(DATA = data_entrada) %>% 
  nest(.by = c(serie, nivel, unidade_nome)) %>% 
  arrange(nivel, unidade_nome, serie)

# 2. FIT GAM (smoothing series) ----

log_msg("Ajustando GAM...")

series_nest <- 
  series_nest %>% 
  mutate(mod_gam = map(data, fit_gam, family = "nb", .progress = TRUE),
         fit_gam = map(mod_gam, get_gam_fitted_values, .progress = TRUE))

# 3. SIMULATION FROM DISTRIBUTION ----

log_msg(paste0("Simulando ", N_SIMS, " amostras a partir dos modelos GAM..."))

# last updated date
max_date <- max(series_mun$data_entrada)

series_nest <- series_nest %>% 
  mutate(gam_sims = map(mod_gam, 
                        sample_from_gam, 
                        n_samples = N_SIMS, 
                        cut_date = max_date %m-% months(3),
                        .progress = TRUE),
         # beta distribution
         beta_distrib = map(gam_sims,
                            get_beta_distrib_from_sims,
                            date = max_date,
                            .progress = TRUE
                            )
         )

log_msg("Determinando percentual de coeficientes em crescimento na última data de observação...")

series_nest <- series_nest %>% 
  mutate(
    beta_perc_asc = map_dbl(
      beta_distrib,
      function(x) {
        if (all(!is.na(x))) {
          return(mean(x$beta > 0, na.rm=TRUE))
        } else {
          return(NA)
        }
      }
    )
  )

# 4. FIT BEAST (trend and changepoints) ----

log_msg("Ajustando BEAST...")

require(xts)

# Unfortunately beast() doesn't support nested tidy operations
# Will have to do with a for loop

series_nest <- series_nest %>%
  mutate(
    serie_xts = map(data, function(x) {xts(x$n, order.by = x$DATA)}, .progress = TRUE)
  )

beast_list <- list()
beast_df_list <- list()

# global params 

global_params <- global_params %>% 
  pivot_wider(names_from = "param", values_from = value) %>% 
  slice_head(n=1)

# serie-level params

series_nest <- series_nest %>% 
  left_join(beast_params, by = c("serie", "nivel"))

for (i in seq_len(nrow(series_nest))) {
  
  log_msg(paste0(i, "/", nrow(series_nest),
                 " (", 100*round(i/nrow(series_nest), 4), "%)"),
          tag = "BEAST")

  trans <- series_nest$trans[i]
  season <- series_nest$season[i]
  detrend <- series_nest$detrend[i]
  deseasonalize <- series_nest$deseasonalize[i]
  
  
    
  beast_tmp <- fit_beast(series_nest$data[i][[1]], 
                         trans = trans,
                         season = season,
                         period = 7,
                         detrend = detrend,
                         deseasonalize = deseasonalize,
                         scp.minmax = c(global_params$scp_min, global_params$scp_max),
                         sorder.minmax = c(global_params$sorder_min, global_params$sorder_max),
                         tcp.minmax = c(global_params$tcp_min, global_params$tcp_max),
                         torder.minmax = c(global_params$torder_min, global_params$torder_max),
                         quiet=TRUE, 
                         print.options = FALSE)
  beast_tmp_df <- beast_tmp %>% tidy_beast(xts_data = series_nest$serie_xts[i][[1]],
                                           trans = trans)
  
  while(all(is.nan(beast_tmp_df$trend_est))) {
    # CHECK IF BEAST RANDOMLY FAILED
    
    log_msg(paste0("Repeating for index ", i))
    beast_tmp <- fit_beast(series_nest$data[i][[1]], 
                           season = "harmonic",
                           period = 7,
                           detrend = TRUE,
                           quiet=TRUE, 
                           print.options = FALSE)
    beast_tmp_df <- beast_tmp %>% tidy_beast(xts_data = series_nest$serie_xts[i][[1]])
  }
  
  if (length(beast_list) == 0) {
    beast_list <- list(beast_tmp)
    beast_df_list <- list(beast_tmp_df)
  } else {
    beast_list[[i]] <- beast_tmp
    beast_df_list[[i]] <- beast_tmp_df
  }
  
  print(beast_df_list[[i]])
}

series_nest$mod_beast <- beast_list
series_nest$beast_df <- beast_df_list

# 4. SAVING RESULTS ----

log_msg("Salvando resultados...")

con_alt <- dbConnect(duckdb::duckdb(),
                     DUCK_DB_ALERTAS,
                     read_only=FALSE
)

## 4.1 GAM Fitted values ----

gam_results <- 
  series_nest %>% 
  select(serie, nivel, unidade_nome, fit_gam) %>% 
  unnest(cols = fit_gam) %>% 
  mutate(fitted_date = Sys.Date(),
         smooth = exp(TREND + CONSTANT)
  ) %>% 
  select(DATA, fitted_date, nivel, unidade_nome, serie, dia_semana, trend = TREND, daily = DAILY, constant = CONSTANT, smooth)

dbWriteTable(con_alt, "tb_fitted", gam_results, append = TRUE)


## 4.2 GAM beta distribution ----

beta_distrib_results <- 
  series_nest %>% 
  select(serie, nivel, unidade_nome, beta_perc_asc) %>% 
  mutate(DATA = max_date) %>% 
  relocate(DATA, .before = everything())

dbWriteTable(con_alt, "tb_beta_distrib", beta_distrib_results, append = TRUE)

## 4.3 BEAST params ----

beast_params <- 
  series_nest %>% 
  select(serie, nivel, trans, season, detrend, deseasonalize) %>% 
  distinct() %>% 
  mutate(fitted_date = Sys.Date()) %>% 
  relocate(fitted_date, .before = everything()) %>% 
  cross_join(global_params)

dbWriteTable(con_alt, "tb_beast_params", beast_params, append = TRUE)

## 4.4 BEAST results ----

beast_results <- 
  series_nest %>% 
  select(serie, nivel, unidade_nome, beast_df) %>% 
  unnest(cols = beast_df) %>% 
  mutate(fitted_date = Sys.Date()) %>% 
  relocate(DATA, fitted_date, nivel, unidade_nome, serie, .before = everything())

dbWriteTable(con_alt, "tb_beast", beast_results, append=TRUE)

dbDisconnect(con_alt, shutdown=TRUE)


log_msg("Concluído.")
rm(list=ls())
