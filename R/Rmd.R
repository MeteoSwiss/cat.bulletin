Rmd_element <- function(filename, envir, elements_dir, clear_page = FALSE, appear = NULL, id = NULL) {
  
  file_path <- file.path(elements_dir, filename)
  assertthat::assert_that(assertthat::is.readable(file_path), msg = paste("Rmd element with filename", filename, "does not exist"))
  log_debug("Using file ", file_path)
  
  assert_that(is.logical(clear_page), length(clear_page) == 1)
  
  Rmd_element <- bulletin_element(type = "Rmd", id = id, appear = appear)
  Rmd_element[["Rmd_file"]] <- file_path
  Rmd_element[["envir"]] <- envir
  Rmd_element[["clear_page"]] <- clear_page  
  Rmd_element
}

#' Add R markdown to a bulletin
#' @param element_id the name of the Rmd file in the elements folder of the package. 
#' @param envir environment in which to knit the Rmd later. 
#' @param clear_page boolean indicating if the page should be cleared after this Rmd element when generating the pdf. 
#' This means that a new page will be started after the output of the Rmd element.
#' @param elements_dir optional path to the directory from which to load the Rmd file. Per default, it will 
#' use the return value of \code{\link{get_default_Rmd_elements_dir}}.
#' @inheritParams bulletin_element 
#' @inheritParams add_element
#' @family bulletin_elements
#' @export
add_Rmd <- function(bulletin, 
                    element_id, 
                    envir = parent.frame(), 
                    clear_page = FALSE, 
                    id = element_id, 
                    appear = c("xml", "pdf"),
                    elements_dir = get_default_Rmd_elements_dir(bulletin)) {
  
  filename <- paste0(paste(bulletin$bulletin_id, element_id, bulletin$language, sep ="_"), ".Rmd")
  log_debug("Adding RMD element with filename", filename, ". 'appear'=", paste(appear, collapse = ","))
  
  assert_that(is.readable(elements_dir), msg = paste0("elements_dir '", elements_dir, "' cannot be read."))
  log_debug("... will look in", elements_dir, "for Rmd element files.")
  
  add_element(bulletin, 
              Rmd_element(filename, 
                          envir = envir, 
                          clear_page = clear_page, 
                          id = id, 
                          appear = appear,
                          elements_dir = elements_dir))
}

#' Get the default directory for Rmd element files
#' @return If the \code{Rmd_elements_dir} property of the bulletin object is set, this setting will be returned as default. 
#' If this is not set, the \code{inst/elements} folder of the \code{cat.bulletin} package will be used. 
#' \code{Rmd_elements_dir} property can be set at creation time using using 
#' the \code{bulletin_args} argument of \code{\link{create_bulletin}} or it can also be set at a later stage. 
#' @param bulletin a bulletin object
#' @export
#' @examples
#' bulletin <- create_bulletin(bulletin_args = list(Rmd_elements_dir = "foo"))  
#' get_default_Rmd_elements_dir(bulletin)
get_default_Rmd_elements_dir <- function(bulletin) {
  if (utils::hasName(bulletin, "Rmd_elements_dir")) {
    bulletin[["Rmd_elements_dir"]]
  } else {
    system.file("elements", package = "cat.bulletin")
  }
}

Rmd_to_markdown_file <- function(element) {
  tmpfile <- tempfile(fileext = ".md")
  #tryCatch({
  quiet = get_log_level() < 2 # be verbose on debug level
  knitr::knit(element[["Rmd_file"]], output = tmpfile, envir = element[["envir"]], quiet = quiet)
  #},
  # warning = function(w) {
  #   log_debug("Warning from knitr::knit in function Rmd_to_markdown_file:", w$message)
  # },
  # error = function(e)
  #   warning(paste("Could not knit Rmd file", element[["Rmd_file"]], "to markdown."))
  # )
  tmpfile
}

Rmd_to_markdown <- function(element, clear_page = element$clear_page) {
  # knit the element markdown file
  tmpfile <- Rmd_to_markdown_file(element) 
  md <- readr::read_lines(tmpfile)
  
  # add a clearpage instruction if needed
  if (clear_page) {
    md <- c(
      md,
      Rmd_clearpage_instruction()
    )
  }
  md
}

Rmd_clearpage_instruction <- function() {
  c(
    "```{=tex}",
    "\\clearpage",
    "\\restoregeometry",
    "```"
  )
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
    warning(paste("Could not render markdown file ", element[["Rmd_file"]], "to html from markdown."))
  )
  html <- readr::read_lines(html_out)
  html <- paste(html, collapse = " ") # join all lines to one
  html
}

Rmd_to_text <- function(element) {
  md_in <- Rmd_to_markdown_file(element) 
  text_out <- tempfile(fileext = ".txt")
  tryCatch({
    markdown::mark(file = md_in, output = text_out, format = "text")
  },
  error = function(e)
    warning(paste("Could not knit markdown file ", element[["Rmd_file"]], "to text from markdown."))
  )
  text <- readr::read_lines(text_out)
  text <- paste(text, collapse = " ") # join all lines to one
  text
}