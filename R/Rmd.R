Rmd_element <- function(filename, envir, appear = NULL, id = NULL) {
  file <- system.file("elements", filename, package = "cat.bulletin")
  assertthat::assert_that(assertthat::is.readable(file), msg = paste("Rmd element with filename", filename, "does not exist"))
  Rmd_element <- bulletin_element(type = "Rmd", id = id, appear = appear)
  Rmd_element[["Rmd_file"]] <- file
  Rmd_element[["envir"]] <- envir
  Rmd_element
}

#' add text to a bulletin
#' @param element_id the name of the Rmd file in the elements folder of the package. 
#' @param envir environment in which to knit the Rmd later. 
#' @inheritParams add_text
#' @export
add_Rmd <- function(bulletin, element_id, envir = parent.frame(), id = element_id, appear = c("xml", "pdf")) {
  filename <- paste0(paste(bulletin$bulletin_id, element_id, bulletin$language, sep ="_"), ".Rmd")
  log_debug("Adding RMD element with filename", filename, ". 'appear'=", paste(appear, collapse = ","))
  add_element(bulletin, Rmd_element(filename, envir = envir, id = id, appear = appear))
}

Rmd_to_markdown_file <- function(element) {
  tmpfile <- tempfile(fileext = ".md")
  tryCatch({
    knitr::knit(element[["Rmd_file"]], output = tmpfile, envir = element[["envir"]])
  },
  error = function(e)
    warning(paste("Could not knit Rmd file", element[["Rmd_file"]], "to markdown."))
  )
  tmpfile
}

Rmd_to_markdown <- function(element) {
  tmpfile <- Rmd_to_markdown_file(element) 
  md <- readr::read_lines(tmpfile)
  md
}

Rmd_to_xml <- function(xml, element, language) {
  html <- Rmd_to_html(element)
  text_node <- assure_node_of_type(xml, type = "text") %>%
    set_languaged_attribute(attribute = "html", language = language, value = html) 
  text_node
}

Rmd_to_html <- function(element) {
  md_in <- Rmd_to_markdown_file(element) 
  html_out <- tempfile(fileext = ".html")
  tryCatch({
      markdown::markdownToHTML(file = md_in, output = html_out, fragment.only = TRUE)
  },
  error = function(e)
    warning(paste("Could not knit markdown file ", element[["Rmd_file"]], "to html from markdown."))
  )
  html <- readr::read_lines(html_out)
  html <- paste(html, collapse = " ") # join all lines to one
  html
}