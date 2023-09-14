library(tidyverse)
library(lubridate)
library(duckdb)

## Paths ----

paths <- read.csv("paths.csv")
db_path <- paths$db_path[1]

DUCK_DB_RUE <- paste0(db_path, "dados_rue.duckdb")
DUCK_DB_CID <- paste0(db_path, "cids.duckdb")
DUCK_DB_ALERTAS <- paste0(db_path, "alertas_rue.duckdb")

# Sample series ----

con_rue <- dbConnect(duckdb(), DUCK_DB_RUE, read_only=TRUE)

# Sindrome gripal 

serie_sg <- con_rue %>% 
  dbGetQuery("select * from vw_series_dia where serie like 'sindrome_gripal' order by data_entrada") %>% 
  as_tibble() %>% 
  rename(DATA = data_entrada)

# Bronquiolite

serie_bronq <- con_rue %>% 
  dbGetQuery("select * from vw_series_dia where serie like 'bronquiolite' order by data_entrada") %>% 
  as_tibble() %>% 
  rename(DATA = data_entrada)

# Arbovirose

serie_arbo <- con_rue %>% 
  dbGetQuery("select * from vw_series_dia where serie like 'arbo' order by data_entrada") %>% 
  as_tibble() %>% 
  rename(DATA = data_entrada)
 
con_rue %>% dbDisconnect(shutdown=TRUE)
 
source("aux_deteccao_tendencias_rue.R")

serie_sg

periodo_interesse <- seq(
  as.Date("2023-03-20"),
  as.Date("2023-06-19"),
  by = "days"
)

modelos_df <- tibble(
  data_limite = periodo_interesse,
  modelo = map(
    data_limite,
    function(x) {
      serie_sg %>% 
        filter(DATA <= x) %>% 
        fit_gam(family = "nb")
    }, .progress = TRUE
  )
)

modelos_df <- modelos_df %>% 
  mutate(fitted = map(modelo, get_gam_fitted_values),
         samples = map(modelo, sample_from_gam, n_samples = 100, cut_date = '2023-03-01')
         )

modelos_df <- modelos_df %>% 
  mutate(beta_distrib = map2(samples, data_limite, get_beta_distrib_from_sims))

modelos_df <- modelos_df %>% 
  mutate(perc_pos = map_dbl(beta_distrib, function(x) {mean(x$beta > 0)}))

fitted_df <- modelos_df %>% 
  select(fitted, data_limite, perc_pos) %>% 
  unnest(fitted) %>% 
  mutate(SMOOTH = exp(TREND + CONSTANT))

mod_atual <- serie_sg %>% 
  fit_gam(family = "nb")

modelos_df <- modelos_df %>% 
  mutate(next_date = data_limite + 1,
         pred = map2_dbl(modelo, next_date, function(x, y) {
           predict(x,
                   tibble(DATA = y,
                          DATA_n = as.numeric(DATA),
                          dia_semana = wday(DATA)
                   )
           ) %>% exp()
         })
  )


serie_sg %>% 
  filter(DATA >= '2023-01-01') %>% 
  ggplot(aes(DATA, n)) + 
  geom_point(shape = 1, alpha = 0.3) + 
  geom_line(
    data = mod_atual %>% 
      get_gam_fitted_values() %>% 
      filter(DATA >= '2023-01-01') %>% 
      mutate(SMOOTH = exp(TREND + CONSTANT)),
    aes(DATA, SMOOTH, group = 1), color = "black", linewidth = 1.6
  ) +
  geom_line(
    data = fitted_df %>% filter(DATA >= '2023-01-01'),
    aes(DATA, SMOOTH, group=data_limite, color=perc_pos), 
    alpha = 0.8
  ) +
  theme_minimal() + 
  scale_color_distiller(palette = "RdYlGn", limits = c(0, 1))




predict(mod_atual, newdata = tibble(DATA = max(serie_sg$DATA) + 1, DATA_n = as.numeric(DATA), dia_semana = wday(DATA))) %>% exp()
