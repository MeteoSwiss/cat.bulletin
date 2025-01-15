#' Create bulletin_metdata
#' @param path a string denoting the CMS path
#' @param title multilanguage string with bulletin titles for each language
#' @param lead multilanguage string with lead text for each language
#' @param image an image element for the lead image
#' @param categories a comma separated string with publication categories
#' @param authors a multilanguage string denoting the authors of the publication
#' @param alias multilanguage vector with alias to use in the path for each language
#' @param keywords multilanguage vector of comma separated strings with keywords for each language
#' 
bulletin_metadata <- function(path = NULL, 
                              alias = NULL, 
                              title = NULL, 
                              lead = NULL, 
                              keywords = NULL, 
                              image = NULL, 
                              categories = c("climate"),
                              authors = NULL
                              ) {
  
  metadata <- list(
    sender = "Climate Analysis Tools (CATs)",
    publication_type = "reportOrBulletin",
    path = path, 
    alias = alias, 
    title = title, 
    lead = lead, 
    keywords = keywords, 
    image = image, 
    categories = categories,
    authors = authors
  )
}

assert_bulletin_metdata <- function(metadata, languages = c("de", "fr", "it", "en")) {
  assert_that(is.list(metadata))
  assert_that(metadata %has_name% c("sender", "publication_type", "path", "alias", "title", "lead", "keywords", "image", "categories", "authors"))
  assert_that(is.null(metadata$path) || assert_that(is.character(metadata$path), length(path) == 1))
  assert_that(is.null(metadata$alias) || assert_multi_language_string(metadata$alias, languages = languages))
  assert_that(is.null(metadata$title) || assert_multi_language_string(metadata$title, languages = languages))
  assert_that(is.null(metadata$lead) || assert_multi_language_string(metadata$lead, languages = languages))
  assert_that(is.null(metadata$keywords) || assert_multi_language_string(metadata$keywords, languages = languages))
  assert_that(is.null(metadata$image) || assert_that(is.list(metadata$image)))
  assert_that(is.null(metadata$categories) || assert_that(is.character(metadata$categories)))
  assert_that(is.null(metadata$authors) || assert_multi_language_string(metadata$authors, languages = languages))            
}

set_metadata <- function(bulletin, metadata) {
  assert_bulletin_metdata(metadata, languages = bulletin$languages)
  bulletin[["metadata"]] <- metadata
  bulletin
}

bulletin_metadata_to_markdown <- function(element) {
  paste0(element$text, "\n")
}

bulletin_metadata_to_xml <- function(xml, element, language) {
  md_in <- tempfile()
  html_out <- tempfile()
  writeLines(text_to_markdown(element), con = md_in)
  markdown::markdownToHTML(file = md_in, output = html_out, fragment.only = TRUE)
  html <- readr::read_lines(html_out)
  text_node <- xml2::xml_add_child(xml, .value = "text")
  xml2::xml_attr(text_node, languaged("html", language)) <- html
  xml
}