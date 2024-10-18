download_realization <- function(bulletin, product, filter, filename, out_path, redownload = FALSE) {
  
  log_info("Downloading realization for product '", product, "' with configuration ", filter_to_string(filter))
  
  if (missing(out_path)) {
    out_path <- bulletin$bulletin_path
    if (has_name(filter, "mediaType")) {
      out_path <- switch(filter$mediaType,
                         "text/plain" = bulletin$data_path,
                         bulletin$image_path
      )
    }
    log_debug("out_path missing, setting to '", out_path, "'.")
  }
  
  filepath <- file.path(out_path, filename)
  if (file.exists(filepath)) {
    log_info("File", filepath, "already exists. Skipping download.")
    return(filepath)
  }
  
  
  retry(cat.func::download_realization(
    product = product,
    filter = filter,
    out_path = out_path,
    filename = filename,
    token_refresher = 
      mch.auth::oltoken_token_refresher(
        oltoken_envvar = "MCHDWH_OL_TOKEN",
        stage = bulletin$stage
      )
  ))
}

filter_to_string <- function(filter) {
  paste(names(filter), filter, sep="=", collapse = ",")
}