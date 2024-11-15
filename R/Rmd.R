Rmd_element <- function(filename, envir) {
  file <- system.file("elements", filename, package = "cat.bulletin")
  assertthat::assert_that(assertthat::is.readable(file))
  Rmd_element <- bulletin_element(type = "Rmd")
  Rmd_element[["Rmd_file"]] <- file
  Rmd_element[["envir"]] <- envir
  Rmd_element
}

#' add text to a bulletin
#' @export
add_Rmd <- function(bulletin, element_id, envir = parent.frame()) {
  filename <- paste0(paste(bulletin$bulletin_id, element_id, bulletin$language, sep ="_"), ".Rmd")
  log_debug("Adding RMD element with filename", filename)
  add_element(bulletin, Rmd_element(filename, envir = envir))
}

Rmd_to_markdown <- function(element) {
  tmpfile <- tempfile()
  knitr::knit(element[["Rmd_file"]], output = tmpfile, envir = element[["envir"]])
  md <- readr::read_lines(tmpfile)
  md
}


Rmd_to_xml <- function(xml, element) {
  md <- Rmd_to_markdown(element)
  md <- paste(md, collapse = "\n")
  xml2::xml_add_child(xml, .value = "text", md)
  xml
}