#' Create bulletin_metdata
#' @param path a string denoting the CMS path
#' @param title multilanguage string with bulletin titles for each language
#' @param lead multilanguage string with lead text for each language
#' @param teaser_image an image element for the teaser image
#' @param teaser_source a multilanguage string denoting the source of the teaser image
#' @param document the filepath to the publication to download 
#' @param categories a comma separated string with publication categories
#' @param authors a multilanguage string denoting the authors of the publication
#' @param alias multilanguage vector with alias to use in the path for each language
#' @param keywords multilanguage vector of comma separated strings with keywords for each language
#' @param publication the path to the publication file
bulletin_metadata <- function(path = NULL, 
                              alias = NULL, 
                              title = NULL, 
                              lead = NULL, 
                              keywords = NULL, 
                              teaser_image = NULL, 
                              teaser_source = NULL,
                              categories = c("climate"),
                              authors = NULL,
                              publication = NULL,
                              publishedAt = Sys.Date()
                              ) {
  
  metadata <- list(
    sender = "Climate Analysis Tools (CATs)",
    publication_type = "reportOrBulletin",
    path = path, 
    alias = alias, 
    title = title, 
    lead = lead, 
    keywords = keywords, 
    teaser_image = teaser_image, 
    teaser_source = teaser_source,
    categories = categories,
    authors = authors, 
    publication = publication,
    publishedAt = publishedAt
  )
  metadata
}

#' @examples 
#' m <- bulletin_metdata()
#' m <- update_metadata_element(m, path = "new path")
#' m <- update_metadata_element(m, title = update_multi_language_string(m$title, "de", "neuer deutscher Titel"))
update_metadata_element <- function(metadata, ...) {
  assert_bulletin_metdata(metadata)
  
  args <- list(...)
  argnames <- names(args)
  for (argname in argnames) {
    if (has_name(metadata, argname)) {
      metadata[[argname]] <- args[[argname]]
    } else {
      warning(paste("Cannot update metadata element", argname))
    }
  }
  metadata
}

assert_bulletin_metdata <- function(metadata, languages = c("de", "fr", "it", "en")) {
  assert_that(is.list(metadata))
  assert_that(metadata %has_name% c("sender", "publication_type", "path", "alias", "title", "lead", "keywords", "teaser_image", "teaser_source", "categories", "authors", "publication", "publishedAt"))
  assert_that(is.null(metadata$path) || assert_that(is.character(metadata$path), length(metadata$path) == 1))
  assert_that(is.null(metadata$alias) || assert_multi_language_string(metadata$alias, languages = languages))
  assert_that(is.null(metadata$title) || assert_multi_language_string(metadata$title, languages = languages))
  assert_that(is.null(metadata$lead) || assert_multi_language_string(metadata$lead, languages = languages))
  assert_that(is.null(metadata$keywords) || assert_multi_language_string(metadata$keywords, languages = languages))
  assert_that(is.null(metadata$teaser_image) || assert_that(is.list(metadata$teaser_image)))
  assert_that(is.null(metadata$teaser_source) || assert_multi_language_string(metadata$teaser_source, languages = languages))
  assert_that(is.null(metadata$categories) || assert_that(is.character(metadata$categories)))
  assert_that(is.null(metadata$authors) || assert_multi_language_string(metadata$authors, languages = languages))
  assert_that(is.null(metadata$publication) || assert_multi_language_string(metadata$publication, languages = languages))      
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