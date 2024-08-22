text_element <- function(text) {
  text_element <- bulletin_element(type = "text")
  text_element[["text"]] <- text
  text_element
}

#' add text to a bulletin
add_text <- function(bulletin, text) {
  add_element(bulletin, text_element(text))
}

text_to_markdown <- function(element) {
  element$text
}


text_to_xml <- function(xml, element) {
  xml2::xml_add_sibling(xml, .value = "text", element$text)
}