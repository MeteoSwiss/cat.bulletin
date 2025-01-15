languaged <- function(text, language) {
  paste(text, language, sep = "_")
}

set_languaged_attribute <- function(xml, attribute, language, value) {
  attribute_name <- languaged(attribute, language)
  xml2::xml_set_attr(xml, attribute_name, value)
  xml
}