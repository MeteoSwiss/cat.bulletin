#' Create publication metdata
#' 
#' The publication metadata contains all the metadata information required for publication as webpage 
#' on the MeteoSwiss website.
#' @param path a string denoting the CMS path
#' @param title multilanguage string with bulletin titles for each language
#' @param lead multilanguage string with lead text for each language
#' @param teaser_image a path to the teaser image. It will be copied to the bulletins image folder during processing.
#' @param teaser_source a multilanguage string denoting the source of the teaser image
#' @param categories a comma separated string with publication categories
#' @param authors a multilanguage string denoting the authors of the publication
#' @param alias multilanguage vector with alias to use in the path for each language
#' @param keywords multilanguage vector of comma separated strings with keywords for each language
#' @param publication multilanguage vector of paths to the publication files (made avaailable as download)
#' @example inst/examples/metadata_examples.R
#' @export
publication_metadata <- function(path = NULL, 
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

#' @details
#' Use \code{update_metadata} to update elements within a publication_metadata list. 
#' For multi language strings, you can use the function \code{\link{update_multi_language_string}}, see examples.
#' @param metadata a publication_metadata list of publication metadata
#' @examples 
#' my_metadata <- publication_metadata()
#' # update an element 
#' my_metadata <- update_metadata_element(my_metadata, path = "new path")
#' # update a language string within a multi language element
#' my_metadata <- update_metadata_element(my_metadata, title = update_multi_language_string(m$title, "de", "neuer deutscher Titel"))
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
  assert_that(is.null(metadata$path) || assert_that(is.character(metadata$path), length(metadata$path) == 1))
  assert_that(is.null(metadata$alias) || assert_multi_language_string(metadata$alias, languages = languages))
  assert_that(is.null(metadata$title) || assert_multi_language_string(metadata$title, languages = languages))
  assert_that(is.null(metadata$lead) || assert_multi_language_string(metadata$lead, languages = languages))
  assert_that(is.null(metadata$keywords) || assert_multi_language_string(metadata$keywords, languages = languages))
  assert_that(is.null(metadata$teaser_image) || assert_that(is.character(metadata$teaser_image)))
  assert_that(is.null(metadata$teaser_source) || assert_multi_language_string(metadata$teaser_source, languages = languages))
  assert_that(is.null(metadata$categories) || assert_that(is.character(metadata$categories)))
  assert_that(is.null(metadata$authors) || assert_multi_language_string(metadata$authors, languages = languages))
  assert_that(is.null(metadata$publication) || assert_multi_language_string(metadata$publication, languages = languages))      
}

lore_ipsum <- function(language) {
  switch(language,
         de = "Damit Ihr indess erkennt, woher dieser ganze Irrthum gekommen ist, und weshalb man die Lust anklagt und den Schmerz lobet, so will ich Euch Alles eröffnen und auseinander setzen, was jener Begründer der Wahrheit und gleichsam Baumeister des glücklichen Lebens selbst darüber gesagt hat. Niemand, sagt er, verschmähe, oder hasse, oder fliehe die Lust als solche, sondern weil grosse Schmerzen ihr folgen, wenn man nicht mit Vernunft ihr nachzugehen verstehe. Ebenso werde der Schmerz als solcher von Niemand geliebt, gesucht und verlangt, sondern weil mitunter solche Zeiten eintreten, dass man mittelst Arbeiten und Schmerzen eine grosse Lust sich zu verschaften suchen müsse. Um hier gleich bei dem Einfachsten stehen zu bleiben, so würde Niemand von uns anstrengende körperliche Übungen vornehmen, wenn er nicht einen Vortheil davon erwartete. Wer dürfte aber wohl Den tadeln, der nach einer Lust verlangt, welcher keine Unannehmlichkeit folgt, oder der einem Schmerze ausweicht, aus dem keine Lust hervorgeht?",
         it = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.",
         fr = "Cependant, afin que vous sachiez d'où vient toute cette erreur, et pourquoi l'on accuse le plaisir et l'on loue la douleur, je vais vous exposer et vous expliquer tout ce qu'a dit à ce sujet le fondateur de la vérité et, pour ainsi dire, l'architecte de la vie heureuse. Personne, dit-il, ne dédaigne, ne hait, ne fuit le plaisir en tant que tel, mais parce que de grandes douleurs le suivent, si l'on ne sait pas le suivre par la raison. De même, personne n'aime, ne recherche et ne désire la douleur en tant que telle, mais parce qu'il arrive parfois que l'on doive chercher à se procurer un grand plaisir au moyen de travaux et de douleurs. Pour s'en tenir ici au plus simple, aucun d'entre nous ne se livrerait à des exercices physiques fatigants s'il n'en attendait un avantage. Mais qui pourrait blâmer celui qui demande un plaisir qui n'est pas suivi d'un désagrément, ou celui qui évite une douleur qui n'est pas suivie d'un plaisir?",
         stop("lore_ipsum: language not implemented")
  )
}