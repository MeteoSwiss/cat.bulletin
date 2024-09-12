#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function() {
  
  bulletin <- create_bulletin() %>%
    monatsbulletin_head() %>%
    monatsbilanz_temp()
  
  #bulletin_pdfxmlzip(bulletin)
  bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
}

monatsbulletin_head <- function(bulletin) {
    add_text(bulletin, paste("# Monatsbulletin", Sys.Date())) %>%
    add_text(paste("normal text")) %>%
    add_image(filepath = system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png"),
              filename = "loess.png",
              caption = "This is a caption.")
}

monatsbilanz_temp <- function(bulletin) {
  
  #input aus anaperiod
  mon = 8
  
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
  
  loess <- evoclim::loess.filt.knmi(x=abs,years=year)
  preind <- 1871:1900
  ipre <- which(year %in% preind)
  mpre <- mean(abs[ipre])
  loesscurr <- as.numeric(loess$fit[poscurr])
  diff <- round(loesscurr-mpre,1)
  
  # homogoval.eval datenfile für august (abs temp und anonmalie)
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-temp_de.Rmd")
  #bulletin <- add_text(bulletin, 
  #                     text = paste0("Die landesweit gemittelte Monatstemperatur im ",month[mon]," ",ycurr," betrug ",vcurr,"°C."))
    
}

