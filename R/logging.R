#' add logging information to the console 
#' 
#' Users can influence the level of output by setting
#' options(log_level)
#' @param log_level numeric 0, 1, or 2 for silent, info or debug
#' @param ... elements of the log message, passed on to \code{paste}
#' @param style optional styling information for message
#' @return no return value
#' @keywords internal
#' @import assertthat
#' @export
#' 
#' @examples 
#' \dontrun{
#' options(log_level = 1) # default
#' log_debug("a debug message not shown")
#' log_info("info messages are shown by default")
#' options(log_level = 2)
#' log_debug("now also debug messages are shown in the output")
#' }
add_log <-function(..., log_level = 1, style = "none") {
  
  assertthat::assert_that(is.numeric(log_level) && log_level >= 0 && log_level <=3)
  
  user_log_level = get_log_level() # log info level per default
  
  # print log only if message log_level >= requested user log level
  if (log_level > user_log_level)
    return()
  
  message <- paste(Sys.time(), ...)

  switch(as.character(style),
         "h1" = cli::cli_h1(message),
         "h2" = cli::cli_h2(message),
         "h3" = cli::cli_h3(message),
         "success" = cli::cli_alert_success(message),
         switch(as.character(user_log_level),
                "1" = cli::cli_alert_info(message),
                cat(message, fill = TRUE)
         )
  )
}

get_log_level <-function() 
  getOption("log_level", default = 1)

#' add a log message at the information level
#'
#' @inheritParams add_log
#' @keywords internal
#' @export
#' 
#' @examples 
#' log_info("this is a message on information level sown per default")
log_info <- function(..., style = "none") {
  add_log("[INFO]", ..., log_level = 1, style = style)
}

#' add a log message at the debug level
#' @inheritParams add_log
#' @keywords internal
#' @export
#' 
#' @examples 
#' log_debug("this is a debug message not shown per default")
log_debug <- function(..., style = "none") {
  add_log("[DEBUG]", ..., log_level = 2, style = style)
}