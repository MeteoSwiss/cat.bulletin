#' Create publication metdata
#' 
#' The publication metadata contains all the metadata information required for publication as webpage 
#' on the MeteoSwiss website.
#' @param path a string denoting the CMS path. If the path does not start with a \code{/}, 
#' it will be interpreted as relative to \code{"/meteoswiss/homepage/service-and-publications/publications/reports-and-bulletins/climate-bulletins/"}.
#' @param title multilanguage string with bulletin titles for each language
#' @param lead multilanguage string with lead text for each language
#' @param teaser_image a path to the teaser image. It will be copied to the bulletins image folder during processing.
#' @param teaser_source a multilanguage string denoting the source of the teaser image
#' @param categories a comma separated string with publication categories. 
#' Possible categories: weather, hazards, climate, measurementAndForecastingSystem.
#' @param authors a multilanguage string denoting the authors of the publication
#' @param alias multilanguage vector with alias to use in the path for each language
#' @param keywords a comma separated strings with keywords 
#' @param publication multilanguage vector of paths to the publication files (made avaailable as download)
#' @param edition multilanguage vector of edition field in xml publication.
#' @param publishedAt Set publication date (visible on the website) in format YYYY-MM-DD, for example "2005-11-15".
#' @example inst/examples/publication_metadata_examples.R
#' @export
publication_metadata <- function(path = NULL, 
                                 alias = NULL, 
                                 title = NULL, 
                                 lead = NULL, 
                                 keywords = NULL, 
                                 teaser_image = NULL, 
                                 teaser_source = NULL,
                                 categories = c("climate"),
                                 authors = "MeteoSchweiz / M\u00e9t\u00e9oSuisse / MeteoSvizzera",
                                 publication = NULL,
                                 publishedAt = Sys.Date(),
                                 edition = NULL
) {
  
  if (!is.null(path) && !startsWith(path, "/"))
    path <- paste0("/meteoswiss/homepage/service-and-publications/publications/reports-and-bulletins/climate-bulletins/", path)
  
  metadata <- list(
    sender = paste0("Climate Analysis Tools (CATs), package cat.bulletin (v", utils::packageVersion("cat.bulletin")),
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
    publishedAt = publishedAt,
    edition = edition
  )
  metadata
}

#' @details
#' Use \code{update_metadata} to update elements within a publication_metadata list. 
#' For multi language strings, you can use the function \code{\link{update_multi_language_string}}, see examples.
#' @param metadata a publication_metadata list of publication metadata
#' @param ... metadata elements to update
#' @examples 
#' my_metadata <- publication_metadata(
#'   title = c(de = "deutscher titel", fr = "titre fran\u00e7ais")
#' )
#' # update an element 
#' my_metadata <- update_metadata_element(my_metadata, path = "/my_new_path")
#' # update a language string within a multi language element
#' my_metadata <- update_metadata_element(my_metadata, title = update_multi_language_string(my_metadata$title, "de", "neuer deutscher Titel"))
#' 
#' @rdname publication_metadata
#' @export 
update_metadata_element <- function(metadata, ...) {
  assert_publication_metdata(metadata)
  
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

assert_publication_metdata <- function(metadata, languages = c("de", "fr", "it", "en")) {
  assert_that(is.list(metadata))
  assert_that(metadata %has_name% c("sender", "publication_type", "path", "alias", "title", "lead", "keywords", "teaser_image", "teaser_source", "categories", "authors", "publication", "publishedAt"))
  assert_that(is.null(metadata$path) || assert_that(is.character(metadata$path), length(metadata$path) == 1, startsWith(metadata$path, "/")))
  assert_that(is.null(metadata$alias) || assert_multi_language_string(metadata$alias, languages = languages))
  assert_that(is.null(metadata$title) || assert_multi_language_string(metadata$title, languages = languages))
  assert_that(is.null(metadata$lead) || assert_multi_language_string(metadata$lead, languages = languages))
  assert_that(is.null(metadata$keywords) || assert_multi_language_string(metadata$keywords, languages = languages))
  assert_that(is.null(metadata$teaser_image) || assert_that(is.character(metadata$teaser_image)))
  assert_that(is.null(metadata$teaser_source) || assert_multi_language_string(metadata$teaser_source, languages = languages))
  assert_that(is.null(metadata$categories) || assert_that(is.character(metadata$categories), length(metadata$categories) == 1))
  assert_that(is.null(metadata$authors) || assert_that(is.character(metadata$authors), length(metadata$authors) == 1))
  assert_that(is.null(metadata$publication) || assert_multi_language_string(metadata$publication, languages = languages))
  assert_that(is.null(metadata$edition) || assert_multi_language_string(metadata$edition, languages = languages))  
}

#' get lore ipsum content
#' @param language language identifier
#' @export
lore_ipsum <- function(language) {
  lore_ipsum_file <- system.file("example-data", "lore-ipsum.txt", package = "cat.bulletin")
  lore_ipsum <- readr::read_lines(lore_ipsum_file)
  names(lore_ipsum) = c("de", "it", "fr")
  unname(lore_ipsum[language])
}