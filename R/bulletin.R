#' Create a bulletin
#' @return an object that represents the bulletin content
#' @export
create_bulletin <- function() {
  list(elements = list())
}

add_element <- function(bulletin, element) {
  bulletin$elements <- append(bulletin$elements, list(element))
  bulletin
}

#' @export
bulletin_to_markdown <- function(bulletin, filename = tempfile(fileext = ".Rmd")) {
  file_conn <- file(filename, open = "wb") # readr::write_lines only supports binary connections
  on.exit(close(file_conn))
  
  for (element in bulletin$elements) {
    lines <- do.call(what = paste0(element$type, "_to_markdown"), args = list(element = element))
    readr::write_lines(lines, file = file_conn)
  }
  
  filename
}

#' @export
bulletin_to_pdf <- function(bulletin, filename = tempfile(fileext = ".pdf")) {
  markdown_file = bulletin_to_markdown(bulletin)
  rmarkdown::render(markdown_file, output_format = "pdf_document", output_file = filename)
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
  tmpdir <- tempdir()
  bulletindir <- file.path(tmpdir, "bulletin")
  dir.create(bulletindir)
  bulletin_to_xml(bulletin, filename = file.path(bulletindir, "bulletin.xml"))
  utils::zip(zipfile = filename, files = bulletindir)
  filename
}