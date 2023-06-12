tidy_beast <- function(beast, xts_data) {
  beast_df <- tibble(
    DATA = index(xts_data),
    Y_obs = beast$data,
    trend_est = beast$trend$Y,
    seasonal_est = beast$season$Y,
    slope_est = beast$trend$slp,
    prob_pos_slope = beast$trend$slpSgnPosPr,
    prob_zero_slope = beast$trend$slpSgnZeroPr,
    prob_neg_slope = 1 - (prob_pos_slope + prob_zero_slope),
    slope_status = case_when(
      prob_pos_slope > prob_zero_slope & prob_pos_slope > prob_neg_slope ~ "ascending",
      prob_neg_slope > prob_zero_slope & prob_neg_slope > prob_pos_slope ~ "descending",
      TRUE ~ "stable"
    ),
    prob_changepoint = beast$trend$cpOccPr,
    is_probable_changepoint = 1:length(xts_data) %in% beast$trend$cp
  )
  beast_df
}