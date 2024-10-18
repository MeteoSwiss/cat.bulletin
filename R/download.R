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

# https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-maps-monthly-prelim
download_monatsbilanz_maps <- function(bulletin,
                                       filename = NULL,
                                       valueBase = c("abs", "anom9120"),
                                       provisional = FALSE,
                                       parameter = c("temp", "prec", "sunshine")) {
  
  valueBase = match.arg(valueBase)
  parameter = match.arg(parameter)
  assertthat::assert_that(is.logical(provisional))
  
  #https://service.meteoswiss.ch/pbbackend/api/v1/products/climate-precipitation-maps-M/realizations?productName=climate-precipitation-maps-M&year=2024&parameter=R&month=09&representation=nostats&valueBase=abs&mediaType=image%2Fpng
  
  attributevalues <- 
    list(
      valueBase = valueBase,
      mediaType = "image/png",
      productName = switch(parameter, 
                           prec = "climate-precipitation-maps-M", 
                           temp = "climate-temperature-maps-M",
                           sunshine = "climate-sunshine-maps-M"),
      parameter = switch(parameter, prec = "R", 
                         temp = "T",
                         sunshine = "S")
    )
  
  #provisional
  if (provisional) {
    attributevalues$productName = paste0(attributevalues$productName, "prelim")
  } else {
    attributevalues = c(attributevalues, c(month = sprintf("%02d", bulletin$month), year = bulletin$year, representation = "nostats"))
  }
  
  download_realization(
    bulletin = bulletin,
    product = attributevalues$productName,
    filter = attributevalues,
    filename = filename
  )
  
}

download_monatsbilanz_temp <- function(bulletin,
                                       filename = NULL,
                                       valueBase = c("abs", "anom"),
                                       provisional = FALSE, 
                                       mediaType = "text/plain") {
  
  valueBase = match.arg(valueBase)
  assertthat::assert_that(is.logical(provisional))
  
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

download_witterungsverlauf <- function(bulletin,
                                       filename = NULL,
                                       month = 10,
                                       year = 2024, 
                                       location = "SMA",
                                       language = "de") {
  
#  valueBase = match.arg(valueBase)

  attributevalues <- 
    list(
      mediaType = "image/png",
      productName = "climate-overview-series-monthdaily"
    )

  attributevalues = c(attributevalues, c(month = sprintf("%02d", bulletin$month), year = bulletin$year, location = location, language = language))
  
  download_realization(
    bulletin = bulletin,
    product = attributevalues$productName,
    filter = attributevalues,
    filename = filename
  )
}