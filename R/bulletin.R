#' Create a bulletin
#' @param bulletin_id a string identifing the type of the bulletin, e.g. \code{bulletin-monthly}
#' @param bulletin_args a list of arguments 
#' @param workdir working directory for bulletin creation
#' @param bulletin_path the path to the directory where the bulletin will be created in
#' @param bulletin_dir the name of the directory within the bulletin_path where bulletin related files will be stored.
#' @param metadata a bulletin_metadata object with metadata for the publication. Can also be set later with \code{\link{set_metadata}}
#' @return an object that represents the bulletin content
#' @export
create_bulletin <- function(bulletin_id,
                            languages = c("de", "en", "fr", "it"),
                            bulletin_args = list(),
                            bulletin_dir = "bulletin", 
                            workdir = tempdir(),
                            bulletin_path = file.path(workdir, bulletin_dir),
                            metadata = bulletin_metadata()
                            ) {
  
  # use a random string for id when no is given (testing purposes)
  if (missing(bulletin_id))
    bulletin_id = randomString()
  
  languages = match.arg(languages, several.ok = TRUE)
  
  bulletin <- bulletin_args
  
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
  bulletin_path <- suppressWarnings(normalizePath(bulletin_path)) # expand ~, ".", etc. 
  bulletin_path <- create_path(bulletin_path)
  
  # prepare data path
  data_dir <- "data"
  data_path <- create_path(bulletin_path, data_dir)
  
  # prepare image path
  image_dir <- "images"
  image_path <- create_path(bulletin_path, image_dir)
  
  # prepare cache path
  cache_dir <- "cache"
  cache_path <- create_path(bulletin_path, cache_dir)
  
  #
  assert_bulletin_metdata(metadata, languages = languagues)
  
  bulletin <- c(bulletin, 
                list(bulletin_id = bulletin_id,
                     bulletin_dir = bulletin_dir,
                     bulletin_path = bulletin_path,
                     data_dir = data_dir,
                     data_path = data_path,
                     image_dir = image_dir,
                     image_path = image_path,
                     cache_dir = cache_dir,
                     cache_path = cache_path,
                     bulletin_envir = new.env(),
                     stage = "prod",
                     languages = languages,
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
#' When compiling input
set_active_language <- function(bulletin, language = bulletin$languages[1]) {
  # set language in cat.lang
  cat.lang::set.language(cat.func::isolang2dwhlang(language))
  # set language in bulletin
  bulletin$language <- language
  bulletin
}

#' Set the metadata object for a bulletin
#' @rdname create_bulletin
#' @inheritParams add_element
#' @param metadata A list of metadata information created with \code{\link{bulletin_metadata}}.
set_metadata <- function(bulletin, metadata) {
  assert_bulletin_metdata(metadata, languages = bulletin$languages)
  bulletin[["metadata"]] <- metadata
  bulletin
}



#' Adds an element to a bulletin
#' @rdname create_bulletin
#' @importFrom set_element
add_element <- function(bulletin, element, language = bulletin$language) {
  slot <- languaged_elements(language)
  bulletin[[slot]] <- append(bulletin[[slot]], list(element))
  bulletin
}

#' Adds or replaces an element in the bulletin
#' @rdname create_bulletin
#' @param bulletin a bulletin created by \code{\link{create_bulletin}}.
#' @param element one of the bulletin elements
#' @importFrom bulletin_element
set_element <- function(bulletin, element, language = bulletin$language, id = element$id) {
  assert_that(is.string(id))
  slot <- languaged_elements(language)
  bulletin[[slot]][[id]] <- element
  bulletin
}

#' Checks if the bulletin has an element in the given language
#' @rdname create_bulletin
#' @inheritParams add_element
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

get_elements <- function(bulletin, language = bulletin$language, type = NULL, id = NULL) {
  if (!is.null(type) && !is.null(id))
    stop("either look for type or id, not both")
  
  slot <- languaged_elements(language)
  
  if (!is.null(type)) {
    types = unique(sapply(bulletin[[slot]], "[[", "type"))
    i <- which(sapply(types, "%in%", type))
    return(bulletin[[slot]][i])
  }
  
  if (!is.null(id)) {
    ids = unique(sapply(bulletin[[slot]], "[[", "id"))
    i <- which(sapply(ids, "%in%", id))
    return(bulletin[[slot]][i])
  }
  
  return(bulletin[[slot]])
}

#' Render a bulletin to markdown
#' 
#' @details 
#' This function will set the current active language to language as a side effect.
#' @inheritParams add_element
#' @param filename The name of the file to write the R markdown to.
#' @export
bulletin_to_markdown <- function(bulletin, 
                                 language = bulletin$language, 
                                 filename = tempfile(pattern = languaged("bulletin", language),
                                                     fileext = ".Rmd")
) {
  bulletin <- set_active_language(bulletin, language)
  
  file_conn <- file(filename, open = "wb") # readr::write_lines only supports binary connections
  on.exit(close(file_conn))
  
  # write the R markdong front matter first
  write_markdown_frontmatter(bulletin = bulletin, file_conn = file_conn)
  
  # add markdown for all elements
  for (element in bulletin[[languaged_elements(language)]]) {
    tryCatch({
      lines <- do.call(what = paste0(element$type, "_to_markdown"), args = list(element = element))
      readr::write_lines(lines, file = file_conn)
    },
    error = function(e) {
      warning_message <- paste("Could not process element", element$id, ":", e)
      warning(warning_message)
      #readr::write_lines(paste('<span style="color:red">',  warning_message, '</span>'),
      #                   file = file_conn)
      #readr::write_lines( paste("Could not process element", element$id),
      #                   file = file_conn)     
    }
    )
  }
  
  filename
}

write_markdown_frontmatter <- function(bulletin, file_conn) {
  front_matter <- c(
    "---"
  )
  
  if (has_element(bulletin, type = "title")) {
    front_matter <- c(
      front_matter,
      paste("title:", get_elements(bulletin, type = "title")[[1]]$title)
    )
  }
  
  babel <- switch(bulletin$language,
                  "de" = "ngerman",
                  "fr" = "french", 
                  "it" = "italian",
                  "en" = "british",
                  stop("unknown language")
  )
  
  front_matter <- c(front_matter,
                    "output:",
                    "  pdf_document:",
                    "    fig_caption: true",
                    "    fig_width: 3",
                    #                    paste0("    lang: ", bulletin$language, "-CH"),
                    "header-includes:",
                    "  - \\usepackage{xcolor}",
                    paste0("  - \\usepackage[", babel, "]{babel}"),
                    #    "    includes:",
                    #    "      in_header: 'preamble.tex',
                    "---"
  )
  readr::write_lines(front_matter, file = file_conn)
}

#' Render a bulletin to pdf
#' @inheritParams add_element
#' @param filename The name of the file to write the pdf.
#' @details 
#' This function will set the current active language to language as a side effect.
#' @export
bulletin_to_pdf <- function(bulletin, 
                            language = bulletin$language, 
                            filename = tempfile(pattern = languaged("bulletin", language),
                                                fileext = ".pdf")
) {
  log_debug("Processing bulletin to pdf via markdown...")
  markdown_file = bulletin_to_markdown(bulletin, language = language)
  log_debug("Processing file", markdown_file, "to pdf.")
  log_debug("Expected pdf-file:", filename)
  rmarkdown::render(markdown_file, envir = bulletin$bulletin_envir, output_format = "pdf_document", output_file = filename, clean = FALSE)
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