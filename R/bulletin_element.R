#' Create a bulletin element
#' @param type a string defining the type of the bulletin element
#' @details 
#' The bulletin element gets an id which is composed of the type and a random string.
bulletin_element <- function(type) {
  list(type = type,
       id = paste(type, randomString(), sep ="_"))
}

randomString <- function(length = 10) {
  rawToChar(as.raw(sample(c(65:90,97:122), size = length, replace=T)))
}