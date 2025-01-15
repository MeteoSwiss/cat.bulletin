languaged <- function(text, language) {
  paste(text, language, sep = "_")
}

set_languaged_attribute <- function(xml, attribute, language, value) {
  attribute_name <- languaged(attribute, language)
  xml2::xml_set_attr(xml, attribute_name, value)
  xml
}

assert_multi_language_string <- function(string, languages = c("de", "fr", "it", "en"), name = NULL) {
  assert_that(!any(is.null(names(string))), msg = "multi language strings must have named elements")
  assert_that(all(names(string) %in% languages), msg = "names of multi language string do not all correspond to language shorts")
}
