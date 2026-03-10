#' Copy a given image to the bulletin image path and return a corresponding image_element
#' @param filepath teaser caption
#' @param filename teaser source
#' @param bulletin the bulletin object
#' @return an image_element of the image
copy_teaser_image <- function(filepath, filename = basename(filepath), bulletin) {
  assert_that(file.exists(filepath))
  newpath <- file.path(bulletin$image_path, filename)
  file.copy(filepath, newpath)
  
  image_element(filename = filename, 
                image_dir = bulletin$image_dir,
                filepath = newpath,
                caption = NULL, 
                alt = NULL, 
                source = NULL, 
                label = NULL)
}


#' Create a teaser_text object
#' @param caption teaser caption
#' @param source teaser source
#' @export
create_teaser_text <- function(caption, source) {
  list(
    caption = caption,
    source = source
  )
}
