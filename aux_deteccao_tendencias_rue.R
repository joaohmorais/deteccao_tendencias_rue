require(magrittr)
require(dplyr)
require(lubridate)
require(tidyr)

# GAM FIT

fit_gam <- function(df, daily_k = 7, daily_bs = "ps", weekly_k = 52, weekly_bs = "cr", family = "gaussian") {
  df <- df %>% 
    mutate(DATA_n = as.numeric(DATA),
           dia_semana = wday(DATA, label=FALSE)) %>% 
    relocate(DATA_n, .after = DATA)
  
  require(mgcv)
  df %>% 
    gam(
      formula = n ~ 
        s(dia_semana, bs = daily_bs, k=daily_k) + 
        s(DATA_n, bs = weekly_bs, k=weekly_k),
      family = family
    )
}

# Define-se uma função `rmnv()` que gera valores aleatório de uma normal multivariada. 
# A função é definida apenas para evitar carregar outro pacote.

rmvn <- function(n, mu, sig) {
  L <- mroot(sig)
  m <- ncol(L)
  t(mu + L %*% matrix(rnorm(n = m*n), m, n))
}

get_gam_sim_interval <- function(mod, n_samples=1000, leave_out = "dia_semana") {
 out_ids <- which(grepl(leave_out, names(coefficients(mod))))
 
 pred_t <- predict(mod, newdata = mod$model, se.fit = TRUE, type = "terms")
 
 Vb <- vcov(mod)[-out_ids,-out_ids]
 
 se.fit <- pred_t$se.fit[,2]
 
 set.seed(42)
 
 BUdiff <- rmvn(n_samples, mu = rep(0, nrow(Vb)), sig = Vb)
 
 Cg <- predict(mod, mod$model, type = "lpmatrix")
 simDev <- Cg[,-out_ids] %*% t(BUdiff)
 
 absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
 
 masd <- apply(absDev, 2L, max)
 
 crit <- quantile(masd, prob = 0.95, type = 8)
 
 tibble(
   mod$model %>% select(DATA_n, dia_semana),
   DATA = as.Date(DATA_n, origin = "1970-01-01"),
   fit = pred_t$fit[,2] + attr(pred_t, "constant"),
   se.fit = pred_t$se.fit[,2],
   uprP = fit + (2 * se.fit),
   lwrP = fit - (2 * se.fit),
   uprS = fit + (crit * se.fit),
   lwrS = fit - (crit * se.fit)
 ) %>% 
   relocate(DATA, .before = DATA_n)
}

sample_from_gam <- function(mod, n_samples = 1000, leave_out = "dia_semana") {
  out_ids <- which(grepl(leave_out, names(coefficients(mod))))
  
  Vb <- vcov(mod)[-out_ids,-out_ids]
  Cg <- predict(mod, mod$model, type = "lpmatrix")
  
  sims <- rmvn(n_samples, mu = coef(mod)[-out_ids], sig = Vb)
  fits <- Cg[,-out_ids] %*% t(sims)
  
  fits %>%
    as.data.frame() %>% 
    stack() %>% 
    tibble() %>% 
    mutate(DATA_n = rep(mod$model$DATA_n, ncol(fits)),
           DATA = as.Date(DATA_n, origin = "1970-01-01")) %>% 
    relocate(DATA, DATA_n, .before = everything())
}

get_betas_from_sims <- function(sims, start_date, end_date, k=4, show_progress=FALSE) {
  sims <- sims %>% 
    filter(DATA > (start_date - k), DATA <= end_date) %>% 
    group_by(ind) %>% 
    arrange(DATA) %>% 
    mutate(r = row_number())
  n <- max(sims$r)
  
  iterations <- matrix(
    1:(n-(k-1)),
    ncol=1
  )
  
  for (i in seq_len(k-1)) {
    iterations <- cbind(
      iterations,
      (1+i):(n-(k-(i+1)))
    )
  }
  
  betas <- tibble(
    r = numeric(),
    est = numeric(),
    inf = numeric(),
    sup = numeric(),
    p_value = numeric(),
    DATA = Date()
  )
  
  for (i in iterations[,1]) {
    if (show_progress) {
      print(paste0("Iteration ", i, "/", n, "...")) 
    }
    sims_sub <- sims %>%
      filter(r %in% iterations[i,])
    lm_mod <- lm(values ~ DATA, data = sims_sub)
    betas <- betas %>%
      add_row(
        tibble(
          r = i,
          est = coefficients(lm_mod)[2],
          inf = confint(lm_mod)[2,1],
          sup = confint(lm_mod)[2,2],
          p_value = summary(lm_mod)$coefficients[2,4],
          DATA = max(sims_sub$DATA)
        )
      )
  }
  
  return(betas)
}

get_beta_distrib_from_sims <- function(sims, date, k=4, show_progress=FALSE) {
  print(date)
  sims <- sims %>% 
    filter(DATA > (as.Date(date) - k), DATA <= date) %>% 
    group_by(ind) %>% 
    arrange(DATA) %>% 
    mutate(r = row_number())
  
  beta_distrib <- tibble(
    ind = factor(x = character(), levels = levels(sims$ind)),
    beta = numeric(),
    DATA = Date()
  )
  
  if (nrow(sims) > 0) {
    for (j in  levels(sims$ind)) {
      if(show_progress) {
        print(paste0("Iteration ", gsub("V", "", j), "/", length(levels(sims$ind))))
      }
      sims_sub <- sims %>%
        filter(ind == j)
      lm_mod <- lm(values ~ DATA, data=sims_sub)
      
      beta_distrib <- beta_distrib %>% 
        add_row(
          ind = j,
          beta = coefficients(lm_mod)[2],
          DATA = max(sims_sub$DATA)
        )
    }  
  }
  
  return(beta_distrib)
}

# SAVE FUNCTIONS ----

save_betas_to_db <- function(betas, db_path, table_name, serie, cid_str, K, n_sims, model_name = "GAM_SIM_INTERVALS_1", append = T, overwrite = F) {
  betas <- betas %>% 
    mutate(SERIE = serie,
           CID_STR = cid_str,
           METHOD = model_name,
           K = K,
           N_SIMS = n_sims,
           STATUS = case_when(
             inf > 0 ~ "crescimento",
             sup < 0 ~ "queda",
             TRUE ~ "estabilidade"
           )) %>% 
    select(
      DATA,
      UNIDADE_NOME,
      SERIE,
      CID_STR,
      METHOD,
      K,
      N_SIMS,
      BETA_EST = est,
      BETA_INF = inf,
      BETA_SUP = sup,
      STATUS
    )
  
  require(duckdb)
  con <- dbConnect(duckdb::duckdb(),
                   db_path,
                   read_only=FALSE)
  dbWriteTable(con, table_name, betas, append = append, overwrite = overwrite)
  
  dbDisconnect(con, shutdown=TRUE)
}

save_beta_distrib_to_db <- function(beta_distrib, db_path, table_name, serie, cid_str, K, model_name = "GAM_SIM_INTERVALS_2", append = T, overwrite = F) {
  beta_distrib <- beta_distrib %>% 
    mutate(SERIE = serie,
           CID_STR = cid_str,
           METHOD = model_name,
           K = K,
           ID_AMOSTRA = as.numeric(gsub("V", "", ind))
           ) %>% 
    select(
      DATA,
      UNIDADE_NOME,
      SERIE,
      CID_STR,
      METHOD,
      K,
      ID_AMOSTRA,
      BETA_EST = beta
    )
  
  require(duckdb)
  con <- dbConnect(duckdb::duckdb(),
                   db_path,
                   read_only=FALSE)
  dbWriteTable(con, table_name, beta_distrib, append = append, overwrite = overwrite)
  
  dbDisconnect(con, shutdown=TRUE)
}
