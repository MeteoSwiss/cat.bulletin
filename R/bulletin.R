#' Create a bulletin
#' @description 
#' The bulletin list ist the top level data strucutre in the \code{cat_bulletin} package. 
#' It contains the bulletin content in form of bulletin_elements, separated for each language. 
#' The bulletin elements represent building blocks of a bulletin like text, R-Markdown sections, links, images etc.
#' The bulletin data structure also contains information needed for bulletin rendering, like working directories etc. 
#' @param bulletin_id a string that identifies the type of the bulletin, e.g. \code{climate-bulletin-monthly}
#' @param bulletin_args a list of arguments 
#' @param languages The set of supported language identifiers for the publication. Must be a subset of \code{de}, \code{fr}, \code{it}, \code{en}.
#' @param workdir working directory for bulletin creation
#' @param bulletin_dir the name of the directory within the bulletin_path where bulletin related files will be stored.
#' @param metadata a publication_metadata object with metadata for the publication. Can also be set later with \code{\link{set_metadata}}.
#' @param pdf_file_base_name a string that will be used for naming generated pdfs for that bulletin. 
#' The name will be constructed by adding "_<language_identifer>.pdf" to the \code{pdf_file_base_name}.
#' @return an object that represents the bulletin content
#' @details 
#' The path where the bulletin artefacts will be put (bulletin_path) will be created within the \code{workdir} and named \code{bulletin_dir}. 
#' @seealso [create_test_bulletin_for_web()] for a minimal example on how to create a bulletin.
#' @export
create_bulletin <- function(bulletin_id,
                            languages = c("de", "en", "fr", "it"),
                            bulletin_args = list(),
                            bulletin_dir = bulletin_id, 
                            workdir = tempdir(),
                            metadata = publication_metadata(),
                            pdf_file_base_name = bulletin_id
) {
  
  # use a random string for id when no is given (testing purposes)
  if (missing(bulletin_id))
    bulletin_id = randomString()
  
  languages = match.arg(languages, several.ok = TRUE)
  
  bulletin <- bulletin_args
  
  assert_that(is.dir(workdir), is.writeable(workdir))
  workdir = suppressWarnings(normalizePath(workdir)) # expand ~, ".", etc. 
  
  create_path <- function(path, subpath = NULL) {
    if (!is.null(subpath)) path <- file.path(bulletin_path, subpath)
    
    if (dir.exists(path)) {
      log_debug(paste0("directory '", path, "' already exists."))
    } else {
      dir.create(path)
    }
    path
  }
  
  # prepare bulletin dir
  bulletin_path <- file.path(workdir, bulletin_dir)
  bulletin_path <- create_path(bulletin_path)
  
  # prepare data path
  data_dir <- "data"
  data_path <- create_path(bulletin_path, data_dir)
  
  # prepare image path
  image_dir <- "images"
  image_path <- create_path(bulletin_path, image_dir)
  
  # prepare files path
  files_dir <- "files"
  files_path <- create_path(bulletin_path, files_dir)
  
  # prepare cache path
  cache_dir <- "cache"
  cache_path <- create_path(bulletin_path, cache_dir)
  
  #
  assert_publication_metdata(metadata, languages = languages)
  
  bulletin <- c(bulletin, 
                list(bulletin_id = bulletin_id,
                     bulletin_dir = bulletin_dir,
                     bulletin_path = bulletin_path,
                     data_dir = data_dir,
                     data_path = data_path,
                     image_dir = image_dir,
                     image_path = image_path,
                     files_dir = files_dir,
                     files_path = files_path,
                     pdf_file_base_name = pdf_file_base_name,
                     cache_dir = cache_dir,
                     cache_path = cache_path,
                     bulletin_envir = new.env(),
                     stage = "prod",
                     languages = languages,
                     language = languages[1],
                     metadata = metadata
                )
  )
  
  elements_slots <- languaged_elements(languages)
  for (slot in elements_slots)
    bulletin[[slot]] <- list()
  
  bulletin <- set_active_language(bulletin)
  bulletin
}

languaged_elements <- function(language) {
  languaged("elements", language)
}

#' Set the active language for the bulletin
#' 
#' The language defines the language environment / settings to use when compiling input or adding elements to a bulletin.
#' @param bulletin The bulletin object created with \code{\link{create_bulletin}}.
#' @param language language identifier.
#' @export
set_active_language <- function(bulletin, language = bulletin$languages[1]) {
  language = match.arg(language, choices = bulletin$languages)
  
  # set language in cat.lang
  cat.lang::set.language(cat.func::isolang2dwhlang(language))
  # set language in bulletin
  bulletin$language <- language
  bulletin
}

#' Set the metadata object for a bulletin
#' @rdname create_bulletin
#' @param metadata A list of metadata information created with \code{\link{publication_metadata}}.
set_metadata <- function(bulletin, metadata) {
  assert_publication_metdata(metadata, languages = bulletin$languages)
  bulletin[["metadata"]] <- metadata
  bulletin
}

#' Adds an element to a bulletin
#' @rdname create_bulletin
add_element <- function(bulletin, element, language = bulletin$language) {
  slot <- languaged_elements(language)
  bulletin[[slot]] <- append(bulletin[[slot]], list(element))
  bulletin
}

#' Adds or replaces an element in the bulletin
#' @rdname create_bulletin
#' @param bulletin a bulletin created by \code{\link{create_bulletin}}.
#' @param element one of the bulletin elements
#' @param language Langue identifier (i.e., "de"). Elements of different languages are kept separate within the bulletin object. 
#' The language parameter specifies the language of the element to be added.
# @inheritParams bulletin_element
set_element <- function(bulletin, element, language = bulletin$language, id = element$id) {
  assert_that(is.string(id))
  slot <- languaged_elements(language)
  bulletin[[slot]][[id]] <- element
  bulletin
}

#' Checks if the bulletin has an element in the given language
#' @rdname create_bulletin
#' @param type string The type of the element(s) 
#' @param id string The id of the element
has_element <- function(bulletin, language = bulletin$language, type = NULL, id = NULL) {
  
  if (!is.null(type) && !is.null(id))
    stop("either look for type or id, not both")
  
  slot <- languaged_elements(language)
  
  if (!is.null(type)) {
    types = unique(sapply(bulletin[[slot]], "[[", "type"))
    
    return(type %in% types)
  }
  
  if (!is.null(id)) {
    ids = sapply(bulletin[[slot]], "[[", "id")
    return(id %in% ids)
  }
  
  length(bulletin[[slot]]) > 0
}

get_elements <- function(bulletin, language = bulletin$language, type = NULL, id = NULL, appear = NULL) {
  if (!is.null(type) && !is.null(id))
    stop("either look for type or id, not both")
  
  slot <- languaged_elements(language)
  
  if (length(bulletin[[slot]]) == 0)
    return(bulletin[[slot]])
  
  elements <- 
    if (!is.null(type)) {
      types = sapply(bulletin[[slot]], "[[", "type")
      i <- which(sapply(types, "%in%", type))
      bulletin[[slot]][i]
    } else  if (!is.null(id)) {
      ids = unique(sapply(bulletin[[slot]], "[[", "id"))
      i <- which(sapply(ids, "%in%", id))
      bulletin[[slot]][i]
    } else {
      bulletin[[slot]]
    }
  
  #appear
  if (!is.null(appear)) {
    appear = match.arg(appear, c("xml", "pdf"), several.ok = TRUE)
    i = which(sapply(elements, function(element) any(element[["appear"]] %in% appear)))
    elements <- elements[i]
  }
  
  return(elements)
}

bulletin_pdfxmlzip <- function(bulletin) {
  
  pdf <- bulletin_to_pdf(bulletin)
  xml <- bulletin_to_xml(bulletin)
  zip <- bulletin_to_webzip(bulletin)
  
  cli::cli_h1("Output:")
  cli::cli_li(paste("pdf:", pdf))
  cli::cli_li(paste("xml:", xml))
  cli::cli_li(paste("zip:", zip))
}

get_locale <- function(language) {
  paste0(language, "_CH.UTF-8")
}