text_element <- function(text, id = NULL, hidden = FALSE) {
  text_element <- bulletin_element(type = "text", id = id, hidden = hidden)
  text_element[["text"]] <- text
  text_element
}

#' add text to a bulletin
#' @export
add_text <- function(bulletin, text, id = NULL, hidden = FALSE) {
  set_element(bulletin, text_element(text, id = id, hidden = hidden))
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
  text_node <- assure_node_of_type(xml, type = "text") %>%
    set_languaged_attribute(attribute = "html", language = language, value = html) 
  text_node
}