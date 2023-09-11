fit_beast <- function(df, trans = "none", ...) {
  require(Rbeast)
  require(xts)
  
  if (!"xts" %in% class(df)) {
    
    if (trans == "log") {
      df <- df %>% 
        mutate(log_n = case_when(
          n > 0 ~ log(n),
          n == 0 ~ 0
        ))
      df <- xts(df$log_n, order.by = df$DATA)
    } else { # no transformation
      df <- xts(df$n, order.by = df$DATA)  
    }
    
  }
  args <- list(...)
  args$y <- df
  args$period <- 7
  do.call("beast", args)
}
