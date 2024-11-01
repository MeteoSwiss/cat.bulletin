#' Create a bulletin
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
  
  language = match.arg(language)
  # set language in cat.lang
  cat.lang::set.language(get_catlang_language_identifier(language))
  
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
  
  c(bulletin, 
    list(bulletin_id = bulletin_id,
         elements = list(),
         bulletin_dir = bulletin_dir,
         bulletin_path = bulletin_path,
         data_path = data_path,
         image_path = image_path,
         bulletin_envir = new.env(),
         stage = "prod",
         language = language
    )
  )
}

get_catlang_language_identifier <- function(language) {
  switch(language,
         "de" = "G",
         "fr" = "F",
         "it" = "I",
         "en" = "E"
  )
}

#' @rdname create_bulletin
#' @param bulletin a bulletin created by \code{\link{create_bulletin}}.
#' @param element one of the bulletin elements
add_element <- function(bulletin, element) {
  bulletin$elements <- append(bulletin$elements, list(element))
  bulletin
}

#' Render a bulletin to markdown
#' @inheritParams add_element
#' @param filename The name of the file to write the R markdown to.
#' @export
bulletin_to_markdown <- function(bulletin, filename = tempfile(fileext = ".Rmd")) {
  file_conn <- file(filename, open = "wb") # readr::write_lines only supports binary connections
  on.exit(close(file_conn))
  
  # write the R markdong front matter first
  write_markdown_frontmatter(file_conn)
  
  # add markdown for all elements
  for (element in bulletin$elements) {
    lines <- do.call(what = paste0(element$type, "_to_markdown"), args = list(element = element))
    readr::write_lines(lines, file = file_conn)
  }
  
  filename
}

write_markdown_frontmatter <- function(file_conn) {
  front_matter <- c(
    "---",
    "output:",
    "  pdf_document:",
    "    fig_caption: true",
    "    fig_width: 5",
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
  
  xml <- xml2::xml_new_root(.value = "title", "Title")
  for (element in bulletin$elements) {
    xml <- do.call(what = paste0(element$type, "_to_xml"), args = list(xml = xml, element = element))
  }
  xml2::write_xml(xml, file = filename)
  filename
}

#' @export
bulletin_to_webzip <- function(bulletin, filename = tempfile(fileext = ".zip")) {
  
  bulletin_to_xml(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.xml"))
  withr::with_dir(new = file.path(bulletin$bulletin_path, ".."),
                  code = utils::zip(zipfile = filename, files = bulletin$bulletin_dir)
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