#' Try to load specific teaser image for given yearmonth. If not successful, return default teaser image for the given month.
#' @inheritParams monthlybulletin_teaser_text
monthlybulletin_teaser_image <- function(yearmonth) {
  image <- tryCatch({
    bulletinpath <- file.path(get_config_value("bulletin_prod_path"), yearmonth)
    find_monthlybulletin_teaser_image(path = bulletinpath)
  },
  error = function(e) {
    month <- substr(yearmonth, 5, 6)
    log_info(paste(e$message, "Using default teaser image for month", month, "."))
    path <- system.file(package = "cat.bulletin", "example-data", "bulletin_monthly", "default_teaser", month)
    find_monthlybulletin_teaser_image(path = path)
  }
  )
  image
}

find_monthlybulletin_teaser_image <- function(path) {
  filepath <- file.path(path, paste0("teaser_image.jpg"))
  if (assertthat::is.readable(filepath)) {
    filepath
  } else {
    stop(paste("Cannot find teaser image file", filepath))
  }
}


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
create_teaser_text <- function(caption, source) {
  list(
    caption = caption,
    source = source
  )
}

#' Try to load specific teaser text for given yearmonth. If not successful, return default teaser for the given month.
#' @param yearmonth single string giving year and month of the bulletin as "yyyymm" 
#' @param language language identifier
monthlybulletin_teaser_text <- function(yearmonth, language) {
  text <- tryCatch({
    bulletinpath <- file.path(get_config_value("bulletin_prod_path"), yearmonth)
    read_monthlybulletin_teaser_text(path = bulletinpath, language = language)
  },
  error = function(e) {
    month <- substr(yearmonth, 5, 6)
    log_info(paste(e$message, "Using default teaser text for month", month, "."))
    path <- system.file(package = "cat.bulletin", "example-data", "bulletin_monthly", "default_teaser", month)
    read_monthlybulletin_teaser_text(path = path, language = language)
  }
  )
  text
}

# Format: 
# first line: caption
# second line: source
# (Old Format: one line with "caption (source)")
read_monthlybulletin_teaser_text <- function(path, language) {
  filepath <- file.path(path, paste0("teaser_", language, ".txt"))
  
  log_debug("reading teaser file", filepath)
  assert_that(is.readable(filepath),
              msg = paste("Cannot read teaser text file", filepath))
  
  lines <- readr::read_lines(filepath)
  if (length(lines) > 2)
    warning(paste("teaser text", filepath, "contains more than two lines. Using only the first two."))
  
  assert_that(length(lines) >= 1,
              msg = paste("teaser file", filepath, "must contain at least 1 line (caption).")
  )
  caption <- lines[1]
  source <- if(length(lines) >= 2) lines[2] else ""
  
  teaser_text <- create_teaser_text(caption = caption, 
                                    source = source)
  teaser_text
}

