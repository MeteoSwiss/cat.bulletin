#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function() {
  
  bulletin <- create_bulletin(bulletin_path = "./bulletin") %>%
    monatsbulletin_head() %>%
    monatsbilanz_temp()
  
  #bulletin_pdfxmlzip(bulletin)
  bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
}

monatsbulletin_head <- function(bulletin) {
  add_text(bulletin, paste("# Monthly Bulletin", Sys.Date())) %>%
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
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-evolution-series-outlook?cg1-static.valueBase=abs&cg1-static.timeGranularity=month&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution-outlook&lang=de
  
  # definitiv  für entsprechenden Monat: climate-temperature-evolution
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-temperature-evolution?cg1-static.valueBase=abs&cg1-static.timeOfYear=08&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution&lang=de
  
  filename <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_abs_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
  data_abs <- read.table(filename, header = TRUE)
  
  # absolute temperature, swissmean
  year <- data_abs$year
  ycurr <- year[length(year)]
  poscurr <- length(year)
  ybeg <- year[1]
  abs  <- data_abs$val
  vcurr <- round(abs[poscurr],1)
  
  
  # homogoval.eval datenfile für august (abs temp und anonmalie)
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-temp_de.Rmd")
  #bulletin <- add_text(bulletin, 
  #                     text = paste0("Die landesweit gemittelte Monatstemperatur im ",month[mon]," ",ycurr," betrug ",vcurr,"°C."))
  
}


