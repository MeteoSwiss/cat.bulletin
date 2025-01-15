text_element <- function(text) {
  text_element <- bulletin_element(type = "text")
  text_element[["text"]] <- text
  text_element
}

#' add text to a bulletin
#' @export
add_text <- function(bulletin, text) {
  add_element(bulletin, text_element(text))
}

text_to_markdown <- function(element) {
  paste0(element$text, "\n")
}

text_to_xml <- function(xml, element, language) {
  md_in <- tempfile()
  html_out <- tempfile()
  writeLines(text_to_markdown(element), con = md_in)
  markdown::markdownToHTML(file = md_in, output = html_out, fragment.only = TRUE)
  html <- readr::read_lines(html_out)
  text_node <- xml2::xml_add_child(xml, .value = "text")
  xml2::xml_attr(text_node, languaged("html", language)) <- html
  xml
}