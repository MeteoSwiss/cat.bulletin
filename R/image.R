image_element <- function(filename, image_dir, filepath, caption, alt, source, label, appear = NULL, id = NULL) {
  element <- bulletin_element(type = "image", appear = appear, id = id)
  element[["caption"]] <- caption
  element[["filename"]] <- filename
  element[["image_dir"]] <- image_dir
  element[["filepath"]] <- filepath
  element[["alt"]] <- alt
  element[["source"]] <- source
  element[["label"]] <- label
  element
}

#' add an image with caption to a bulletin
#' 
#' copies the image from the filepath to the bulletin directory and adds an image element
#' @param filepath the path of the image to add
#' @param filename the name of the image within the bulletin (can differ from the filepath)
#' @param caption image caption in the bulletin
#' @param alt alt text to use for html / xml export
#' @param source image source (a small text)
#' @param label optional string to identify the image. Use \code{\\@ref(label)} for creating cross references.
#' @family bulletin_element#' 
#' @inheritParams bulletin_element 
#' @inheritParams add_element
#' @examples 
#' image_filepath = system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_abs_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
#' bulletin <- create_bulletin() %>%
#'   add_image(filepath = image_filepath, caption = "An example figure.")
#' @export
add_image <- function(bulletin, filepath, filename = basename(filepath), 
                      caption = NULL, alt = NULL, source = NULL, label = NULL,
                      appear = c("xml", "pdf"),
                      id = NULL) {
  assert_that(file.exists(filepath))
  newpath <- file.path(bulletin$image_path, filename)
  if (file.exists(newpath)) {
    log_debug("add_image: file", bulletin$image_dir, "/", filename, "alread exists. Using existing file.")
  } else {
    file.copy(filepath, newpath)
  }
  add_element(bulletin, image_element(filename = filename, image_dir = bulletin$image_dir, 
                                      filepath = newpath, caption = caption, 
                                      alt = alt, source = source, label = label, id = id,
                                      appear = appear))
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

image_to_xml <- function(xml, element, language) {
  image_node <- assure_node_of_type(xml, type = "image") %>%
    set_languaged_attribute("fileName", language, paste0(element$image_dir, "/", element$filename)) %>%
    set_languaged_attribute("legend", language, element$caption) %>%
    set_languaged_attribute("alt", language, element$alt) %>%
    set_languaged_attribute("source", language, element$source)
  xml2::xml_set_attr(image_node, "hasLightbox", "true") 
  image_node
}

#' Joins two or more images in horizontal or vertical diretion
#' @param image_filepaths a vector of image source filepaths to join
#' @param direction direction of join, either 'horizontal' or 'vertical'.
#' @inheritParams assert_image_outpath
#' @return the path to the output file
#' @export
#' @examples
#' outpath <- join_images(
#'   image_filepaths = rep(system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png"), 2),
#'   direction = "horizontal"
#' )
join_images <- function(image_filepaths, outpath = NULL, direction = c("horizontal", "vertical")) {
  direction = match.arg(direction)
  
  assert_that(all(sapply(image_filepaths, is.readable)))
  
  outpath <- assert_image_outpath(outpath)
  
  convert_args <- paste0(
    switch(direction, 
           horizontal = "+",
           vertical = "-"
    ),
    "append ",
    paste(image_filepaths, collapse = " "),
    " ", 
    outpath
  )
  log_debug("Calling convert to join images with arguments: '", convert_args, "'.")
  tryCatch({
    system2("convert", convert_args)
  },
  error = function(e) {
    stop("Error during join of images.")
  }
  )
  assert_that(is.readable(outpath), msg = "join images: output was not generated")
  
  outpath
}

#' Get width and heigth of an image
#' @param image_filepath an image source filepath
#' @return a named numeric vector with width and height of the image
#' @export
#' @examples
#' get_image_size(
#'   image_filepath = system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png")
#' )
get_image_size <- function(image_filepath) {
  assert_that(is.readable(image_filepath))
  
  args <- paste(
    "-format '%wx%h'",
    image_filepath
  )
  
  log_debug("Calling identify to get image size with arguments: '", args, "'.")
  size <- tryCatch({
    size_string <- system2("identify", args = args, stdout = TRUE)
    size <- unlist(strsplit(size_string, split = "x"))
    size <- as.numeric(size)
    names(size) <- c("width", "height")
    size
  },
  error = function(e) {
    stop(paste("Error during get_image_size for image ", image_filepath))
  }
  )
  size
}

#' Crop an image 
#' @inheritParams get_image_size
#' @inheritParams assert_image_outpath
#' @param side, either "top", "bottom", "left", or "right".
#' @param margin Margin size as a positive number of pixels. Can also be specified relatively as percentage of total width resp. height. Use a string like "20\%" for this.
#' @return the path to the output file
#' @export
#' @examples
#' outpath <- crop_image(
#'   image_filepath = system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png"),
#'   side = "top",
#'   margin = "5%"
#' )
crop_image <- function(image_filepath,
                       outpath = NULL, 
                       side = c("top", "bottom", "left", "right"),
                       margin) {
  assert_that(is.readable(image_filepath))
  outpath <- assert_image_outpath(outpath)
  side <- match.arg(side)
  
  assert_that(!missing(margin), !is.null(margin))
  
  # calculate relative margen
  relative <- FALSE
  if (is.character(margin)) {
    if (endsWith(margin, "%")) {
      margin <- tryCatch(
        as.numeric(substr(margin, 0, nchar(margin)-1)),
        error = function(e)
          stop("Could not interpret relative margin")
      )
      relative <- TRUE
    }
  } 
  assert_that(is.number(margin),
              length(margin) == 1,
              margin >= 0,
              msg = "incorrect margin definition"
  )
  
  size <- get_image_size(image_filepath)
  
  if (relative) {
    margin <- if (side %in% c("left", "right")) {
      margin/100 * size["width"]
    } else {
      margin/100 * size["height"]
    }
    margin <- round(margin)
  }
  
  crop_arg <- switch(side,
                     "top" = paste0(size["width"], "x",
                                   size["height"] - margin,
                                   "+", 0,
                                   "+", margin),
                     "bottom" = paste0(size["width"], "x",
                                    size["height"] - margin,
                                    "+", 0,
                                    "+", 0),
                     "left" = paste0(size["width"] - margin, "x",
                                     size["height"],
                                       "+", margin,
                                       "+", 0),
                     "right" = paste0(size["width"] - margin, "x",
                                      size["height"],
                                      "+", 0,
                                      "+", 0),
                     stop("side not implemented")
  )
  
  args <- paste(
    "-crop",
    crop_arg, 
    image_filepath,
    outpath
  )
  log_debug("Calling convert to crop image with arguments: '", args, "'.")
  tryCatch({
    system2("convert", args)
  },
  error = function(e) {
    stop(paste("Error during crop of image.", e$message))
  }
  )
  assert_that(is.readable(outpath), msg = "crop image: output was not generated")
  outpath
}

string_contains <- function(string, pattern) {
  length(grep("pattern", string) > 0)
}

#' Assert that a filepath to write to is writeable and absolut.
#' @param outpath a single absolute filepath to write the output to. 
#' If only a filename is given, the output is written to \code{temdir()}. 
#' If outpath is NULL, output will be set to a temporary file
#' @return an absolute path to write the image output
#' @keywords internal
assert_image_outpath <- function(outpath = NULL) {
  if (is.null(outpath)) outpath = tempfile()
  assert_that(is.string(outpath))
  
  if (!startsWith(outpath, "/")) {
    log_debug("outpath'", outpath, "'does not seem to be an absolute path.")
    if (length(grep("/", outpath)) > 0)
      stop("image operation: outpath must be absolute path or filename only")
    # seems to be a single filename -> adding tempdir
    outpath <- file.path(tempdir(), outpath)
    log_debug("image operation: adding tempdir to outpath: ", outpath)
  }
  assert_that(
    is.writeable(dirname(outpath)), 
    msg = "outpath not valid or not writeable"
  )
  outpath
}
