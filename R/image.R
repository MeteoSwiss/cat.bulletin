image_element <- function(filepath, filename = basename(filepath), caption, alt, source, label) {
  element <- bulletin_element(type = "image")
  element[["caption"]] <- caption
  element[["filename"]] <- filename
  element[["filepath"]] <- filepath
  element[["alt"]] <- alt
  element[["source"]] <- source
  element[["label"]] <- label
  element
}

#' add an image with caption to a bulletin
#' 
#' copies the image from the filepath to the bulletin directory and adds an image element
#' @param bulletin the bulletin object to which to append the image element
#' @param filepath the path of the image to add
#' @param filename the name of the image within the bulletin (can differ from the filepath)
#' @param caption image caption in the bulletin
#' @param alt alt text to use for html / xml export
#' @param source image source (a small text)
#' @param label optional string to identify the image. Use \code{\\@ref(label)} for creating cross references.
#' @examples 
#' image_filepath = system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_abs_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
#' bulletin <- create_bulletin() %>%
#'   add_image(filepath = image_filepath, caption = "An example figure.")
#' @export
add_image <- function(bulletin, filepath, filename = basename(filepath), 
                      caption = NULL, alt = NULL, source = NULL, label = NULL) {
  assert_that(file.exists(filepath))
  newpath <- file.path(bulletin$bulletin_path, filename)
  file.copy(filepath, newpath, overwrite = TRUE)
  add_element(bulletin, image_element(filename = filename, filepath = newpath, caption = caption, 
                                      alt = alt, source = source, label = label))
}

image_to_markdown <- function(element) {
  # try to use knitr::include_graphics(rep("images/knit-logo.png", 3)) in an knitr junk!
  # see also https://bookdown.org/yihui/rmarkdown-cookbook/figure-placement.html
  # or https://bookdown.org/yihui/rmarkdown-cookbook/figure-size.html
  # control size: ![A nice image.](foo/bar.png){width=50%}
  label <- if (!is.null(element$label)) paste0("\\label{", element$label, "}") else ""
  paste0("![", element$caption, " ", label, "](", element$filepath, '){width=50%,pos="h"}', "\n",
         element$source, "\n")
}

image_to_markdown2 <- function(element) {
  tmpfile <- tempfile()
  knitr::knit(element[["Rmd_file"]], output = tmpfile, envir = element[["envir"]])
  md <- readr::read_lines(tmpfile)
  md
}

image_to_xml <- function(xml, element, language) {
  image_node <- xml2::xml_add_child(xml, .value = "image")
  image_node <- image_node %>%
    set_languaged_attribute("image", language, element$filename) %>%
    set_languaged_attribute("legend", language, element$caption) %>%
    set_languaged_attribute("alt", language, element$alt) %>%
    set_languaged_attribute("source", language, element$source) %>%
    set_languaged_attribute("hasLightbox", language, "true")   
}