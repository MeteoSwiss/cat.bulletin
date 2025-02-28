title_element <- function(title) {
  title_element <- bulletin_element(type = "title")
  title_element[["title"]] <- title
  title_element
}

#' add title to a bulletin
#' @export
add_title <- function(bulletin, title) {
  if (has_element(bulletin, type = "title"))
    stop("This bulletin already has a title element.")
  add_element(bulletin, title_element(title))
}

title_to_markdown <- function(element) {
  element$title
}


title_to_xml <- function(xml, element) {
  xml2::xml_add_child(xml, .value = "title", element$title)
  xml
}