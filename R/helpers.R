languaged <- function(text, language) {
  paste(text, language, sep = "_")
}

languaged_filename <- function(filename, language, ending) {
  paste0(languaged(filename, language), ".", ending)
}

set_languaged_attribute <- function(xml, attribute, language, value) {
  attribute_name <- languaged(attribute, language)
  xml2::xml_set_attr(xml, attribute_name, value)
  xml
}

is_multi_language_string <- function(string, languages = c("de", "fr", "it", "en")) {
  all(sapply(languages, function(language) has_name(string, language)))
}

assert_multi_language_string <- function(string, languages = c("de", "fr", "it", "en"), name = NULL) {
  assert_that(!any(is.null(names(string))), msg = "multi language strings must have named elements")
  assert_that(all(names(string) %in% languages), msg = "names of multi language string do not all correspond to language shorts")
}

empty_multi_language_string <- function(languages) {
  string <- rep("", length(languages))
  names(string) <- languages
  string
}

#' replace a language string in a multilanguage string
#' @param string multilanguage string
#' @param language language to update
#' @param value value to set for the given language
#' @export
update_multi_language_string <- function(string, language, value) {
  assert_multi_language_string(string)  
  assert_that(language %in% names(string))
  string[language] <- value
  string
}
