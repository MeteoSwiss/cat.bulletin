Rmd_element <- function(filename, envir, id = NULL) {
  file <- system.file("elements", filename, package = "cat.bulletin")
  assertthat::assert_that(assertthat::is.readable(file))
  Rmd_element <- bulletin_element(type = "Rmd", id = id)
  Rmd_element[["Rmd_file"]] <- file
  Rmd_element[["envir"]] <- envir
  Rmd_element
}

#' add text to a bulletin
#' @export
add_Rmd <- function(bulletin, element_id, envir = parent.frame(), id = element_id) {
  filename <- paste0(paste(bulletin$bulletin_id, element_id, bulletin$language, sep ="_"), ".Rmd")
  log_debug("Adding RMD element with filename", filename)
  add_element(bulletin, Rmd_element(filename, envir = envir, id = id))
}

Rmd_to_markdown <- function(element) {
  tmpfile <- tempfile(fileext = ".md")
  knitr::knit(element[["Rmd_file"]], output = tmpfile, envir = element[["envir"]])
  md <- readr::read_lines(tmpfile)
  md
}

Rmd_to_xml <- function(xml, element, language) {
  md_in <- tempfile(fileext = ".md")
  html_out <- tempfile(fileext = ".html")
  writeLines(Rmd_to_markdown(element), con = md_in)
  markdown::markdownToHTML(file = md_in, output = html_out, fragment.only = TRUE)
  html <- readr::read_lines(html_out)
  html <- paste(html, collapse = " ") # join all lines to one
  text_node <- assure_node_of_type(xml, type = "text") %>%
    set_languaged_attribute(attribute = "html", language = language, value = html) 
  text_node
}