title_element <- function(title) {
  title_element <- bulletin_element(type = "title")
  title_element[["title"]] <- title
  title_element
}

#' add title to a bulletin
#' @export
add_title <- function(bulletin, title) {
  if (has_element(bulletin, type = "title"))
    stop(paste("The bulletin already has a title element for language", bulletin$language))
  add_element(bulletin, title_element(title))
}

title_to_markdown <- function(element) {
  element$title
}

title_to_xml <- function(xml, element, language) {
  # title is added as attribute to root node
  xml2::xml_attr(xml, languaged("title", language)) <- element$title
  xml
}