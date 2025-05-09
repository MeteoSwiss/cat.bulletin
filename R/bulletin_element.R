#' Create a bulletin element
#' @param type a string defining the type of the bulletin element
#' @param appear vector of output formats the element is used for. One or both of "xml", "pdf".
#' @param id a string giving a unique identifier. The id must be unique within one language but can (should) 
#' be the same across languages. During xml generation, elements with the same id are processed into the same xml node.
#' The default is a composition of type and a random string.
bulletin_element <- function(type, id, appear = c("xml", "pdf")) {
  if (missing(id) || is.null(id))
    id <- generate_element_id(type = type)
  if (!is.null(appear))
    appear = match.arg(appear, several.ok = TRUE)
  list(type = type,
       id = id,
       appear = appear)
}

randomString <- function(length = 10) {
  rawToChar(as.raw(sample(c(65:90,97:122), size = length, replace=T)))
}

generate_element_id <- function(type) {
  paste(type, randomString(), sep ="_")
}

bulletin_element_is_of_type <- function(element, type) {
  element[["type"]] == type
}