image_element <- function(filepath, filename = basename(filepath), caption) {
  element <- bulletin_element(type = "image")
  element[["caption"]] <- caption
  element[["filename"]] <- filename
  element[["filepath"]] <- filepath
  element
}

#' add an image with caption to a bulletin
#' 
#' copies the image from the filepath to the bulletin directory and adds an image element
#' @param bulletin the bulletin object to which to append the image element
#' @param filepath the path to the image to add
#' @param filename the name of the image within the bulletin (can differ from the filepath)
#' @param caption image caption in the bulletin
#' @export
add_image <- function(bulletin, filepath, filename = basename(filepath), caption = NULL) {
  assert_that(file.exists(filepath))
  newpath <- file.path(bulletin$bulletin_path, filename)
  file.copy(filepath, newpath, overwrite = TRUE)
  add_element(bulletin, image_element(filename = filename, filepath = newpath, caption = caption))
}

image_to_markdown <- function(element) {
  # try to use knitr::include_graphics(rep("images/knit-logo.png", 3)) in an knitr junk!
  paste0("![", element$caption, " \\label{fig1}](", element$filepath, ")", "\n",
         element$caption, "\n")
}

image_to_markdwon2 <- function(element) {
  tmpfile <- tempfile()
  knitr::knit(element[["Rmd_file"]], output = tmpfile, envir = element[["envir"]])
  md <- readr::read_lines(tmpfile)
  md
  
}

image_to_xml <- function(xml, element) {
  image_node <- xml2::xml_add_sibling(xml, .value = "image")
  xml2::xml_add_child(image_node, .value = "filename", element$filename)
  xml2::xml_add_child(image_node, .value = "caption", element$caption)
  image_node
}