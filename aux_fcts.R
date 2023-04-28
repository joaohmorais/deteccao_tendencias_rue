require(magrittr)

current_time_str <- function(time_zone = "America/Sao_Paulo", secs = FALSE) {
  
  format_str <- ifelse(secs, "%H:%M:%S", "%H:%M")
  
  return(
    Sys.time() %>% 
      format(format_str, tz = time_zone)
  )
}

log_msg <- function(message) {
  msg <- ifelse(
    exists("i") & exists("n_series"),
    paste0("[", i, "/", n_series, "] "),
    ""
  )
  msg <- paste0(msg, 
                current_time_str(secs=TRUE),
                " - ",
                message)
  print(msg)
}
