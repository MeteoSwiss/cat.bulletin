#' Create the monthly bulletin
#' @param year Bulletin year
#' @param month Bulletin month
#' @param provisional boolean indicating if the provisional version of the bulletin shall be created
#' @param ... further general bulletin arguments forwarded to the create_bulletin function. Use them to set working directory etc. 
#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function(year = 2024, month = 8, provisional = TRUE, ...) {
  
  bulletin <- create_bulletin(bulletin_args = list(year = year,
                                                   month = month,
                                                   provisional = provisional),
                              ...)
  
  bulletin <- bulletin %>%
    monatsbulletin_head() %>%
    monatsbilanz_temp() %>%
    monatsbilanz_precip() %>%
    monatsbilanz_sun() %>%
    temporal_evolution() %>%
    monatsbulletin_disclaimer() %>%
    monatsbulletin_more_info()
  
  #bulletin_pdfxmlzip(bulletin)
  bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
}

monatsbulletin_head <- function(bulletin) {
  add_text(bulletin, paste("# Klimabulletin", Sys.Date())) 
}

monatsbilanz_temp <- function(bulletin) {
  
  #input aus anaperiod
  mon = bulletin$month
  provisional = bulletin$provisional
  
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
  
  
  
  # fix: könnte aus cat.lang gelesen werden
  month <- c("Januar","Februar","März","April","Mai","Juni",
             "Juli","August","September","Oktober","November","Dezember")
  
  # provisorisch: climate-evolution-series-outlook monthly daten file
  # Absolutwerte:
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-evolution-series-outlook?cg1-static.valueBase=abs&cg1-static.timeGranularity=month&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution-outlook&lang=de
  # Anomalie:
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-evolution-series-outlook?cg1-static.valueBase=anom&cg1-static.timeGranularity=month&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution-outlook&lang=de 
  
  # definitiv  für entsprechenden Monat: climate-temperature-evolution
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-temperature-evolution?cg1-static.valueBase=abs&cg1-static.timeOfYear=08&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution&lang=de
  
  # Tables:
  # https://rmarkdown.rstudio.com/lesson-7.html, knitr::kable
  
  # Download data
  #filename_abs <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_abs_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
  filename_abs <- download_monatsbilanz_temp(bulletin, valueBase = "abs", provisional = provisional, filename = "monatsbilanz_temp_abs.txt")
  data_abs <- read.table(filename_abs, header = TRUE)
  
  #filename_anom <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_anom_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
  filename_anom <- download_monatsbilanz_temp(bulletin, valueBase = "anom", provisional = provisional, filename = "monatsbilanz_temp_anom.txt")
  data_anom <- read.table(filename_anom, header = TRUE)
  
  # absolute temperature, swissmean
  year <- data_abs$year
  poscurr <- length(year)
  ycurr <- year[poscurr]
  ybeg <- year[1]
  abs  <- data_abs$val
  vcurr <- round(abs[poscurr],1)
  
  # anomaly temperature, swissmean
  anom <- data_anom$val
  acurr <- round(anom[poscurr],1)
  
  # regional rankings    
  ranking <- sort.int(anom,decreasing=T,index.return=T)
  rankcurr <- which(ranking$ix==poscurr)
  
  if (rankcurr != 1) {
    ind01 <- ranking$ix[1]
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
  } else {
    ind01 <- ranking$ix[2]
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
  }
  
  # years similar to current
  diffc_t5 <- abs(ranking$x[which(ranking$ix==poscurr)]-ranking$x[1:5])
  if (any(diffc_t5<0.1)) {
    isim <- which(diffc_t5<0.1)
    isimy <- ranking$ix[isim]
    isimy <- isimy[-which(isimy==poscurr)]
  } else {
    isim <- isimy <- NULL
  }
  
  # homogoval.eval datenfile für august (abs temp und anonmalie)
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-temp_de.Rmd")
  
  # Add images 
  filename = "monatsbilanz_temp_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_temp(bulletin, valueBase = "abs", provisional = provisional, mediaType = "image/png", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
  filename = "monatsbilanz_temp_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_temp(bulletin, valueBase = "anom", provisional = provisional, mediaType = "image/png", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
  # example table
  regdata_table <- regdata_example_table(bulletin)
  bulletin <- add_flextable(bulletin, flextable = regdata_table)
  
  bulletin
}

regdata_example_table <- function(bulletin) {
  regdata <- readRDS(system.file("example-data", "bulletin_monthly", "regdata-example.Rdata", package = "cat.bulletin"))
  df <- as.data.frame(regdata)
  df <- format(df)
  df$region <- rownames(regdata)
  df <- df[,c(4,1:3)]  # set column order
  
  table <- flextable(df) %>%
    set_header_labels(values =c("Region", "Mittelwert", "Minimum", "Maximum")) %>%
    add_header_row(
      values = c("", "Temperaturen"),
      colwidths = c(1,3)
    ) %>%
    bg(i = ~ as.numeric(TTanom_mean) < 0, j = "TTanom_mean", bg = "#EFEFEF", part = "body") %>%
    add_footer_lines("Example footer line") %>%
    set_caption("Regional temperature data") %>%
    set_table_properties(layout = "autofit")
  table
}

monatsbilanz_precip <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-precip_de.Rmd")
}

monatsbilanz_sun <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-sun_de.Rmd")
}

temporal_evolution <- function(bulletin) {
  mon = bulletin$month
  
  # fix: könnte aus cat.lang gelesen werden
  month <- c("Januar","Februar","März","April","Mai","Juni",
             "Juli","August","September","Oktober","November","Dezember")
  
  filename_abs <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_abs_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
  data_abs <- read.table(filename_abs, header = TRUE)
  
  filename_anom <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_anom_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
  data_anom <- read.table(filename_anom, header = TRUE)
  
  # absolute temperature, swissmean
  year <- data_abs$year
  poscurr <- length(year)
  ycurr <- year[poscurr]
  ybeg <- year[1]
  abs  <- data_abs$val
  
  # loess trend
  loess <- evoclim::loess.filt.knmi(x=abs,years=year)
  preind <- 1871:1900
  ipre <- which(year %in% preind)
  mpre <- mean(abs[ipre])
  loesscurr <- as.numeric(loess$fit[poscurr])
  diff <- round(loesscurr-mpre,1)
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_temporal-evolution_de.Rmd")
  
}

monatsbulletin_more_info <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_more-info_de.Rmd")
}

monatsbulletin_disclaimer <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_disclaimer_de.Rmd")
}


