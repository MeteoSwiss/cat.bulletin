#' Create a bulletin element
#' @param type a string defining the type of the bulletin element
#' @param id a string giving a unique identifier. The id must be unique within one language but can (should) 
#' be the same across languages. During xml generation, elements with the same id are processed into the same xml node.
#' The default is a composition of type and a random string.
bulletin_element <- function(type, id) {
  if (missing(id) || is.null(id))
    id <- generate_element_id(type = type)
  list(type = type,
       id = id)
}

randomString <- function(length = 10) {
  rawToChar(as.raw(sample(c(65:90,97:122), size = length, replace=T)))
}

generate_element_id <- function(type) {
  paste(type, randomString(), sep ="_")
}