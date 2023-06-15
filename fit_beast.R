fit_beast <- function(df, ...) {
  require(Rbeast)
  require(xts)
  
  if (!"xts" %in% class(df)) {
    df <- xts(df$n, order.by = df$DATA)
  }
  args <- list(...)
  args$y <- df
  args$period <- 7
  do.call("beast", args)
}
