#' Pipe graphics
#'
#' Like dplyr, cat.bulletin also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' \dontrun{
#' # Instead of
#' bulletin <- add_image(bulletin, ...)
#' # you can write
#' bulletin <- bulletin %>% add_image(...)
#' }
NULL