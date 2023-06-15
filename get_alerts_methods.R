get_alerts_method_1 <- function(beta_df) {
  beta_df %>% 
    mutate(alert = case_when(
      sup < 0 ~ "desc",
      inf > 0 ~ "asc",
      TRUE ~ "stable"
    ), 
    method = "GAM NB",
    prob_signif = 1-p_value) %>% 
    select(DATA, slope_est = est, prob_signif, alert, method)
}

get_alerts_method_2 <- function(beast_df) {
  beast_df %>% 
    mutate(alert = case_when(
      slope_status == "ascending" ~ "asc",
      slope_status == "descending" ~ "desc",
      TRUE ~ "stable"
    ),
    prob_signif = case_when(
      slope_est > 0 ~ prob_pos_slope,
      slope_est < 0 ~ prob_neg_slope
    ),
    method = "BEAST 1"
    ) %>% 
    select(DATA, slope_est, prob_signif, alert, method)
}

get_alerts_method_3 <- function(beast_df) {
  require(quantreg)
  q_coef <- rq(beast_df$slope_est ~ 1, tau = c(0.2, 0.5, 0.8)) %>% predict() %>% 
    as_tibble() %>% 
    suppressWarnings() %>% 
    rename(inf = 1, median = 2, sup = 3) %>% 
    mutate(DATA = beast_df$DATA)
  
  beast_df %>% 
    mutate(
      alert = case_when(
        slope_est > q_coef$sup[1] ~ "asc",
        slope_est < q_coef$inf[1] ~ "desc",
        TRUE ~ "stable"
      ),
      prob_signif = case_when(
        slope_est > 0 ~ prob_pos_slope,
        slope_est < 0 ~ prob_neg_slope
      ),
      method = "BEAST 2") %>% 
    select(DATA, slope_est, prob_signif, alert, method)
}

get_alerts_method_4 <- function(beast_df) {
  beast_df %>% 
    mutate(
      alert = case_when(
        prob_zero_slope > 0.75 ~ "stable",
        prob_pos_slope > prob_neg_slope ~ "asc",
        prob_neg_slope > prob_pos_slope ~ "desc",
        TRUE ~ "stable"
      ), 
      prob_signif = case_when(
        slope_est > 0 ~ prob_pos_slope,
        slope_est < 0 ~ prob_neg_slope
      ), 
      method = "BEAST 3") %>%
    select(DATA, slope_est, prob_signif, alert, method)
}
