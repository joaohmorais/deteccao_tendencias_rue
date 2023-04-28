require(duckdb)
require(magrittr)
require(dplyr)
require(janitor)
require(lubridate)
require(aweek)

get_serie_from_CIDs <- function(db_path, cids, daily=FALSE, per_unit = FALSE, dictionary = NULL, aggregate_series=TRUE, nchar_descr = 4) {
  cid_str <- paste0("'", paste(cids, collapse = "|"), "'")
  
  query_n <- paste0("SELECT
  ", ifelse(daily, "data_entrada AS DATA,", ""), "
      week(data_entrada + 1) AS SEMANA_EPI,
      CAST(LEFT(yearweek(data_entrada + 1), 4) AS INT) AS ANO_EPI,
  ", ifelse(per_unit, "unidade_nome AS UNIDADE_NOME, ", ""))
  
  
  
  if (aggregate_series) {
    query_n <- paste0(
      query_n,
      "SUM(CASE WHEN regexp_matches(cid_str, ", cid_str,") THEN 1 ELSE 0 END) AS n,
      "
    )
  } else {
    query_cids <- ""
    for (cid in cids_selec) {
      
      cid_name <-  gsub(".", "", cid, fixed=TRUE)
      
      if (!is.null(dictionary)) {
        # Busca descricao e limpa texto
        
        cid_name <- dictionary %>% 
          filter(SUBCAT == cid_name) %>% 
          pull(DESCRICAO) %>% 
          strsplit(split = " ") %>% 
          unlist() %>% .[1:min(nchar_descr, length(.))] %>% 
          paste(collapse = "_") %>%  
          janitor::make_clean_names()
      } 
      
      query_cids <- paste0(
        query_cids,
        "SUM(CASE WHEN regexp_matches(cid_str, '", cid, "') THEN 1 ELSE 0 END) AS ", cid_name, ",
        "
      )
    }
    query_n <- paste0(query_n, query_cids)
  }
  
  query_n <- paste0(
    query_n, 
    "COUNT(*) AS n_tot,
  	  FROM tb_agrega_rue
  	  WHERE UNIDADE_NOME IS NOT NULL 
  	  AND UNIDADE_NOME NOT LIKE 'HOSPITAL MUNICIPAL RONALDO GAZOLLA'
    	GROUP BY SEMANA_EPI, ANO_EPI", 
    ifelse(daily, ", DATA", ""),
    ifelse(per_unit, ", UNIDADE_NOME", ""),"
    	ORDER BY ANO_EPI ASC, SEMANA_EPI ASC",
    ifelse(daily, ", DATA ASC", "")
  )
  
  con <- dbConnect(duckdb::duckdb(),
                   db_path,
                   read_only=TRUE)
  
  serie <- dbGetQuery(con,
                      query_n) %>% 
    as_tibble()
  
  if (!daily) {
    serie <- serie %>% 
      mutate(DATA = aweek::get_date(week = SEMANA_EPI,
                                    year = ANO_EPI,
                                    day=7L))
  }
  
  
  dbDisconnect(con, shutdown=TRUE)
  
  serie
}