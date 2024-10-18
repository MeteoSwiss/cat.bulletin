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

download_monatsbilanz_temp <- function(bulletin,
                                       filename = NULL,
                                       valueBase = "abs", 
                                       provisional = FALSE, 
                                       mediaType = "text/plain") {
  attributevalues <- 
    list(
      valueBase = valueBase,
      normalPeriod= "1991-2020",
      location= "regSwiss",
      language = "de",
      plotPeriod = "1864-today",
      mediaType = mediaType
    )
  
  if (provisional) {
    download_realization(
      bulletin = bulletin,
      product = "climate-temperature-evolution-outlook",
      filter = c(attributevalues, list(timeGranularity="month")),
      filename = filename
    )
  } else {
    download_realization(
      bulletin = bulletin,
      product = "climate-temperature-evolution",
      filter = c(attributevalues, list(timeOfYear = sprintf("%02d", bulletin$month))),
      filename = filename
    )
  }
}