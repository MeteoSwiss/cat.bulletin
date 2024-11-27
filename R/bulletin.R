#' Create a bulletin
#' @param bulletin_id a string identifing the type of the bulletin, e.g. \code{bulletin-monthly}
#' @param bulletin_args a list of arguments 
#' @param workdir working directory for bulletin creation
#' @param bulletin_path the path to the directory where the bulletin will be created in
#' @param bulletin_dir the name of the directory within the bulletin_path where bulletin related files will be stored.
#' @return an object that represents the bulletin content
#' @export
create_bulletin <- function(bulletin_id,
                            language = c("de", "en", "fr", "it"),
                            bulletin_args = list(),
                            bulletin_dir = "bulletin", 
                            workdir = tempdir(),
                            bulletin_path = file.path(workdir, bulletin_dir)) {
  
  # use a random string for id when no is given (testing purposes)
  if (missing(bulletin_id))
    bulletin_id = randomString()
  
  language = match.arg(language)
  # set language in cat.lang
  cat.lang::set.language(cat.func::isolang2dwhlang(language))
  
  bulletin <- bulletin_args
  
  # prepare bulletin dir
  bulletin_path <- normalizePath(bulletin_path)
  dir.create(bulletin_path)
  
  # prepare data path
  data_path <- file.path(bulletin_path, "data")
  dir.create(data_path)
  
  # prepare image path
  image_path <- file.path(bulletin_path, "images")
  dir.create(image_path)
  
  # prepare cache path
  cache_path <- file.path(bulletin_path, "cache")
  dir.create(cache_path)
  
  c(bulletin, 
    list(bulletin_id = bulletin_id,
         elements = list(),
         bulletin_dir = bulletin_dir,
         bulletin_path = bulletin_path,
         data_path = data_path,
         image_path = image_path,
         cache_path = cache_path,
         bulletin_envir = new.env(),
         stage = "prod",
         language = language
    )
  )
}

#' @rdname create_bulletin
#' @param bulletin a bulletin created by \code{\link{create_bulletin}}.
#' @param element one of the bulletin elements
add_element <- function(bulletin, element) {
  bulletin$elements <- append(bulletin$elements, list(element))
  bulletin
}

#' @rdname create_bulletin
#' @inheritParams add_element
#' @param type string The type of the element(s) 
#' @param id string The id of the element
has_element <- function(bulletin, type = NULL, id = NULL) {
  
  if (!is.null(type) && !is.null(id))
    stop("either look for type or id, not both")
  
  if (!is.null(type)) {
    types = unique(sapply(bulletin$elements, "[[", "type"))
    
    return(type %in% types)
  }
  
  if (!is.null(id)) {
    ids = sapply(bulletin$elements, "[[", "id")
    return(id %in% ids)
  }
  
  length(bulletin$elements) > 0
}

get_elements <- function(bulletin, type = NULL, id = NULL) {
  if (!is.null(type) && !is.null(id))
    stop("either look for type or id, not both")
  
  if (!is.null(type)) {
    types = unique(sapply(bulletin$elements, "[[", "type"))
    i <- which(sapply(types, "%in%", type))
    return(bulletin$elements[i])
  }
  
  if (!is.null(id)) {
    ids = unique(sapply(bulletin$elements, "[[", "id"))
    i <- which(sapply(ids, "%in%", id))
    return(bulletin$elements[i])
  }
  
  return(bulletin$elements)
}

#' Render a bulletin to markdown
#' @inheritParams add_element
#' @param filename The name of the file to write the R markdown to.
#' @export
bulletin_to_markdown <- function(bulletin, filename = tempfile(fileext = ".Rmd")) {
  file_conn <- file(filename, open = "wb") # readr::write_lines only supports binary connections
  on.exit(close(file_conn))
  
  # write the R markdong front matter first
  write_markdown_frontmatter(bulletin = bulletin, file_conn = file_conn)
  
  # add markdown for all elements
  for (element in bulletin$elements) {
    tryCatch({
      lines <- do.call(what = paste0(element$type, "_to_markdown"), args = list(element = element))
      readr::write_lines(lines, file = file_conn)
    },
    error = function(e)
      warning(paste("Could not process element", element$id, ":", e))
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
  
  front_matter <- c(front_matter,
                    "output:",
                    "  pdf_document:",
                    "    fig_caption: true",
                    "    fig_width: 3",
                    "header-includes:",
                    "  - \\usepackage{xcolor}",
                    #    "    includes:",
                    #    "      in_header: 'preamble.tex',
                    "---"
  )
  readr::write_lines(front_matter, file = file_conn)
}

#' Render a bulletin to pdf
#' @inheritParams add_element
#' @param filename The name of the file to write the pdf.
#' @export
bulletin_to_pdf <- function(bulletin, filename = tempfile(fileext = ".pdf")) {
  log_debug("Processing bulletin to pdf via markdown...")
  markdown_file = bulletin_to_markdown(bulletin)
  log_debug("Processing file", markdown_file, "to pdf.")
  rmarkdown::render(markdown_file, envir = bulletin$bulletin_envir, output_format = "pdf_document", output_file = filename, clean = FALSE)
}

#' @export
bulletin_to_xml <- function(bulletin, filename = tempfile(fileext = ".xml")) {
  xml <- xml2::xml_new_root(.value = "root")
  root <- xml2::xml_root(xml)
  for (element in bulletin$elements) {
    log_debug("processing element", element$id)
    xml <- do.call(what = paste0(element$type, "_to_xml"), args = list(xml = xml2::xml_root(xml), element = element))
  }
  xml2::write_xml(xml2::xml_root(xml), file = filename)
  filename
}

#' @export
bulletin_to_webzip <- function(bulletin, filename = tempfile(fileext = ".zip")) {
  bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
  bulletin_to_xml(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.xml"))
  withr::with_dir(new = file.path(bulletin$bulletin_path, ".."),
                  code = utils::zip(zipfile = filename, 
                                    files = c(file.path(bulletin$bulletin_dir, "bulletin.xml"),
                                              file.path(bulletin$bulletin_dir, "bulletin.pdf"),
                                              file.path(bulletin$bulletin_dir, "images")
                                    )
                  )
  )
  filename
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