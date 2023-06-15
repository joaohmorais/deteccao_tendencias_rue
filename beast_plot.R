beast_trend_plot <- function(beast_df) {
  beast_df %>% 
    ggplot(aes(x=DATA)) + 
    geom_point(aes(y=Y_obs), shape=1, color="grey60", alpha = 0.4) + 
    geom_line(aes(y=trend_est, group=1, color=slope_status)) + 
    geom_point(data = . %>% filter(is_probable_changepoint), aes(DATA, trend_est, size=prob_changepoint)) + 
    labs(x="Data", y = "Casos diários", title = "Tendência") +
    guides(size="none", color="none") + 
    scale_size_continuous(range = c(1, 4)) +
    scale_color_manual(values = c("red", "green", "black")) +
    theme_bw() + theme(plot.title = element_text(hjust=0.5))
}

beast_season_plot <- function(beast_df) {
  beast_df %>% 
    ggplot(aes(x=DATA, y=seasonal_est)) + 
    geom_line(aes(group=1)) + 
    labs(x="Data", y = "Casos diários", title = "Sazonalidade") +
    theme_bw() + theme(plot.title = element_text(hjust=0.5))
}

beast_slope_plot <- function(beast_df) {
  
  require(quantreg)
  q_coef <- rq(beast_df$slope_est ~ 1, tau = c(0.2, 0.5, 0.8)) %>% predict() %>% 
    as_tibble() %>% 
    rename(inf = 1, median = 2, sup = 3) %>% 
    mutate(DATA = beast_df$DATA) %>% 
    suppressWarnings()
  
  beast_df %>% 
    ggplot(aes(x=DATA, y=slope_est)) + 
    geom_line(aes(group=1)) + 
    geom_hline(yintercept = q_coef$inf[1], linetype = "dashed", color = "green") +
    geom_hline(yintercept = q_coef$median[1], linetype = "dashed", color = "royalblue") +
    geom_hline(yintercept = q_coef$sup[1], linetype = "dashed", color = "red") + 
    coord_cartesian(ylim = c(-2, 2)) + 
    labs(x="Data", y = "Slope", title = "Slope") +
    theme_bw() + theme(plot.title = element_text(hjust=0.5))
}

beast_prob_plot <- function(beast_df) {
  beast_df %>% 
    select(DATA, prob_neg_slope, prob_pos_slope, prob_zero_slope) %>% 
    pivot_longer(cols = starts_with("prob"), names_to = "slope", values_to = "prob", names_prefix = "prob_") %>% 
    ggplot(aes(x=DATA, y=prob)) + 
    geom_area(aes(group=slope, fill=slope)) + 
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) +
    labs(x="Data", y="Probabilidade") +
    scale_fill_manual(values = c("forestgreen", "firebrick", "lightblue4"),
                      name = "Probabilidade", 
                      labels = c("Inclinação negativa", "Inclinação positiva", "Inclinação nula")
    ) +
    theme(legend.position = "top")
}

beast_plot <- function(beast_df) {
  require(gridExtra)
  grid.arrange(
    beast_df %>% beast_trend_plot(),
    beast_df %>% beast_season_plot(),
    beast_df %>% beast_slope_plot(),
    beast_df %>% beast_prob_plot(),
    ncol=1
  )
}

beast_smooth_slope <- function(beast_df, method = "gam", k_factor = 4, supsmu_span = "cv", supsmu_bass = -10, smooth_kind = "3RS3R", loess_span = 0.1) {
  
  if (method == "gam") {
    beast_df %>% 
      select(DATA, n=slope_est) %>% 
      fit_gam(family = "gaussian", weekly_k = k_factor*53) %>% 
      get_gam_fitted_values() %>% 
      mutate(slope_smooth = TREND + CONSTANT)
  #} else if (method == "holtwinters") {
    # start_year <- year(beast_df$DATA[1])
    # start_day <- yday(beast_df$DATA[1])
    # 
    # slope_ts <- ts(beast_df$slope_est, 
    #                start = c(start_year, start_day),
    #                frequency = 365
    #               )
    # hw <- HoltWinters(slope_ts)
    # fitted(hw)
    
  } else if (method == "supersmooth") {
    supsmu(beast_df$DATA, beast_df$slope_est, span = supsmu_span, bass = supsmu_bass) %>% 
      as_tibble() %>% 
      rename(DATA = 1, slope_smooth = 2)
  } else if (method == "smooth") {
    tibble(DATA=beast_df$DATA,
           slope_smooth = smooth(beast_df$slope_est, kind = smooth_kind))
  } else if (method == "loess") {
    tibble(
      DATA = beast_df$DATA,
      slope_smooth = loess(slope_est ~ as.numeric(DATA), data=beast_df, span = loess_span) %>% fitted()
    )
  }
  
  
  
}

beast_smooth_slope_plot <- function(beast_df, method="gam", inf = 0.2, sup = 0.8, k_factor = 4, supsmu_span = "cv", supsmu_bass = -10, smooth_kind = "3RS3R", loess_span = 0.1) {
  slope_smooth_df <- beast_df %>% beast_smooth_slope(method=method, k_factor = k_factor, 
                                                     supsmu_span = supsmu_span, 
                                                     supsmu_bass = supsmu_bass, 
                                                     smooth_kind = smooth_kind, 
                                                     loess_span = loess_span)
  
  qreg_slope_2 <- rq(slope_smooth_df$slope_smooth ~ 1, tau = c(inf, 0.5, sup)) %>% predict() %>% 
    as_tibble() %>% 
    rename(inf = 1, median = 2, sup = 3) %>% 
    mutate(DATA = slope_smooth_df$DATA) %>% 
    suppressWarnings()
  
  slope_smooth_df %>% 
    ggplot(aes(x=DATA, y=slope_smooth)) + 
    geom_line() + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    labs(x="Data", y = "Slope", title = "Smoothed slope") +
    geom_hline(yintercept = qreg_slope_2$inf[1], linetype = "dashed", color = "green") + 
    geom_hline(yintercept = qreg_slope_2$sup[1], linetype = "dashed", color = "red") + 
    theme_bw() + theme(plot.title = element_text(hjust=0.5))
}

beast_new_alert_plot <- function(beast_df, serie, method="gam", inf = 0.2, sup = 0.8,  k_factor = 4, supsmu_span = "cv", supsmu_bass = -10, smooth_kind = "3RS3R", loess_span = 0.1) {
  
  slope_smooth_df <- beast_df %>% beast_smooth_slope(method=method, k_factor = k_factor, 
                                                     supsmu_span = supsmu_span, 
                                                     supsmu_bass = supsmu_bass, 
                                                     smooth_kind = smooth_kind, 
                                                     loess_span = loess_span)
  
  qreg_slope_2 <- rq(slope_smooth_df$slope_smooth ~ 1, tau = c(inf, 0.5, sup)) %>% predict() %>% 
    as_tibble() %>% 
    rename(inf = 1, median = 2, sup = 3) %>% 
    mutate(DATA = slope_smooth_df$DATA) %>% 
    suppressWarnings()
  
  beast_df %>% 
    left_join(slope_smooth_df %>%  select(DATA, slope_smooth)) %>% 
    left_join(serie %>% fit_gam(family="nb") %>% get_gam_fitted_values() %>% mutate(SMOOTH = exp(TREND+CONSTANT)) %>% select(DATA, SMOOTH)) %>% 
    mutate(
      alert = case_when(
        slope_smooth > qreg_slope_2$sup[1] ~ "asc",
        slope_smooth < qreg_slope_2$inf[1] ~ "desc",
        TRUE ~ "stable"
      )
    ) %>% 
    ggplot(aes(x=DATA)) + 
    geom_point(aes(y=Y_obs), color = "grey60", shape=1, alpha=0.3) + 
    geom_line(aes(y=SMOOTH, group=1, color=alert)) + 
    labs(x="Data",  y = "Casos diários", title = "Alert results") +
    theme_bw() + theme(legend.position = "top", plot.title = element_text(hjust=0.5))
}

beast_plot_smooth_alert <- function(beast_df, serie, method="gam", inf = 0.2, sup = 0.8, k_factor = 4, supsmu_span = "cv", supsmu_bass = -10, smooth_kind = "3RS3R", loess_span = 0.1) {
  smooth_slope_plot <- beast_smooth_slope_plot(beast_df, method=method, inf = inf, sup = sup, k_factor = k_factor, 
                                               supsmu_span = supsmu_span, 
                                               supsmu_bass = supsmu_bass, 
                                               smooth_kind = smooth_kind, 
                                               loess_span = loess_span)
  new_alert_plot <- beast_new_alert_plot(beast_df, serie, method=method, inf = inf, sup = sup, k_factor = k_factor, 
                                         supsmu_span = supsmu_span, 
                                         supsmu_bass = supsmu_bass, 
                                         smooth_kind = smooth_kind, 
                                         loess_span = loess_span)
  slope_plot <- beast_slope_plot(beast_df)
  gridExtra::grid.arrange(
    new_alert_plot,
    smooth_slope_plot,
    slope_plot,
    ncol=1
  )
}
