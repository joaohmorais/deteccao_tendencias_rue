library(tidyverse)
library(lubridate)
library(duckdb)

## Paths ----

paths <- read.csv("../paths.csv")
db_path <- paths$db_path[1]

DUCK_DB_RUE <- paste0(db_path, "dados_rue.duckdb")
DUCK_DB_CID <- paste0(db_path, "cids.duckdb")
DUCK_DB_ALERTAS <- paste0(db_path, "alertas_rue.duckdb")

con_rue <- dbConnect(duckdb(), DUCK_DB_RUE, read_only=T)

series_rue <- con_rue %>% 
  dbGetQuery(
    "select distinct serie from vw_series_dia order by serie"
  ) %>% pull(1)

datas_rue <- c(
  con_rue %>% dbGetQuery("select min(data_entrada) from vw_series_dia") %>% pull(1),
  con_rue %>% dbGetQuery("select max(data_entrada) from vw_series_dia") %>% pull(1)
)

dbDisconnect(con_rue, shutdown=TRUE)