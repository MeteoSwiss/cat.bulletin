#' Download a realization from the ProductBrowser ProductProvider
#' @param bulletin the bulletin object
#' @param filter argument for \code{\link[cat.func]{download_realization}}
#' @param product,out_path,filename arguments for \code{\link[cat.func]{download_realization}}
#' @param redownload boolean if \code{FALSE} (default), the realization is not downloaded if the file already exists.
#' @param ... further parameters for \code{\link[cat.func]{download_realization}}
#' @examples
#' \dontrun{
#' download_realization(
#' bulletin = create_bulletin(),
#' product = "climate-precipitation-maps-Ynorm",
#' filter = list(
#'  "productName" = "climate-precipitation-maps-Ynorm",
#'  "valueBase" = "diff",
#'  "timeGranularity" = "Y",
#'  "parameter" = "R",
#'  "normalPeriod" = "1991-2020",
#'  "mediaType" = "image/png",
#'  "representation" = "nostats"
#' ))
#' }
#' @export
download_realization <- function(bulletin, 
                                 product, 
                                 filter, 
                                 filename, 
                                 out_path, 
                                 redownload = FALSE,
                                 ...) {
  
  log_info("Downloading realization for product '", product, "' with configuration ", filter_to_string(filter))
  
  if (missing(out_path)) {
    out_path <- bulletin$bulletin_path
    if (has_name(filter, "mediaType")) {
      out_path <- switch(filter$mediaType,
                         "text/plain" = bulletin$data_path,
                         bulletin$cache_path
      )
    }
    log_debug("out_path missing, setting to '", out_path, "'.")
  }
  
  # skip download if file already exists (only works if filename given)
  if (missing(filename) || is.null(filename)) {
    log_debug("download_realization: Cannot check if file already exists because filename not given.")
    filename = NULL
  } else {
    filepath <- file.path(out_path, filename)
    if (file.exists(filepath) && !redownload) {
      log_info("File '", filepath, "' already exists. Skipping download.")
      return(filepath)
    }
  }
  
  retry(cat.func::download_realization(
    product = product,
    filter = filter,
    out_path = out_path,
    filename = filename,
    ...)
  )
}

filter_to_string <- function(filter) {
  paste(names(filter), filter, sep="=", collapse = ",")
}
