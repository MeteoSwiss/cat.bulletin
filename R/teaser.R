

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

read_monthlybulletin_teaser_text <- function(path, language) {
  filepath <- file.path(path, paste0("teaser_", language, ".txt"))
  text <- if (assertthat::is.readable(filepath)) {
    lines <- readLines(filepath)
    if (length(lines) > 1)
      warning(paste("teaser text", filepath, "contains more than one line. Using only the first."))
    lines[1]
  } else {
    stop(paste("Cannot read teaser text file", filepath))
  }
  text
}

find_monthlybulletin_teaser_image <- function(path) {
  filepath <- file.path(path, paste0("teaser_image.jpg"))
  image <- if (assertthat::is.readable(filepath)) {
    teaser_image(
      filepath = filepath
    )
  } else {
    stop(paste("Cannot find teaser image file", filepath))
  }
  image
}
