retry <- function(..., times = 5) {
  if (times == 0)
    stop(paste0("Repeatedly failed to evaluate expression."))
  
  tryCatch(eval(...),
           error = function(e) {
             log_debug(paste("caught exception", e, ". Retrying..."))
             retry(..., times = times - 1)
           })
  
}