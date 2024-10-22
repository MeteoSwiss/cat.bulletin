#' Create the monthly bulletin
#' @param year Bulletin year
#' @param month Bulletin month
#' @param provisional boolean indicating if the provisional version of the bulletin shall be created
#' @param ... further general bulletin arguments forwarded to the create_bulletin function. Use them to set working directory etc. 
#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function(year = 2024, month = 8, provisional = FALSE, ...) {

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
    monatsbulletin_daily_timeseries() %>%
    monatsbulletin_disclaimer() %>%
    monatsbulletin_more_info()
  
  #bulletin_pdfxmlzip(bulletin)
  bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
}

monatsbulletin_head <- function(bulletin) {
  add_text(bulletin, paste("# Klimabulletin", Sys.Date()))
  add_text(bulletin, paste("Im Leadtext Reihenfolge der zu nennenden Parameter über die Ränge entscheiden. Super wären Sätze im Sinne von DER AUGUST 2024 WAR GEPRÄGT VON HOHEN TEMPERATUREN UND WENIG NIEDERSCHLAG."))

  basepath <- "/prod/zue/climate/basic_serv/information/klimabulletin/klimabulletin_automatisch/"
  
  if (bulletin$month < 10) { 
    monpath <- paste0("0",bulletin$month)
  } else {
    monpath <- bulletin$month
  }
  monpath <- paste0(bulletin$year,monpath)
  teasertext <- readLines(paste0(basepath,monpath,"/",monpath,"_teaser_text.txt"), n = 1)
  
  add_image(bulletin, filename = paste0(monpath,"_teaser_image.jpg"), filepath = paste0(basepath,monpath,"/",monpath,"_teaser_image.jpg"), caption=teasertext)

}

monatsbilanz_temp <- function(bulletin) {
  
  #input aus anaperiod
  mon = bulletin$month
  provisional = bulletin$provisional
  
  lang <- "G"
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  if (lang != "G"){
    month <- sapply(month,add_article)
    month <- as.character(month)
  }
  
  # Download data
  filename_abs <- download_monatsbilanz_temp(bulletin, valueBase = "abs", provisional = bulletin$provisional, filename = "monatsbilanz_temp_abs.txt")
  data_abs <- read.table(filename_abs, header = TRUE)

  filename_anom <- download_monatsbilanz_temp(bulletin, valueBase = "anom", provisional = bulletin$provisional, filename = "monatsbilanz_temp_anom.txt")
  data_anom <- read.table(filename_anom, header = TRUE)

  # absolute temperature, swissmean
  year <- data_abs$year
  poscurr <- length(year)
  ycurr <- year[poscurr]
  ybeg <- year[1]
  abs  <- data_abs$val
  vcurr <- round(abs[poscurr],1)

  vcurr_t <- format(vcurr, nsmall=1)

  # anomaly temperature, swissmean
  anom <- data_anom$val
  acurr <- round(anom[poscurr],1)
  acurr_t <- format(acurr, nsmall=1)

  # rank swissmean
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
  reca_t <- format(reca, nsmall=1)
  recval_t <- format(recval, nsmall=1)

  # years similar to current
  diffc_t5 <- abs(ranking$x[which(ranking$ix==poscurr)]-ranking$x[1:5])
  if (any(diffc_t5<0.1)) {
    isim <- which(diffc_t5<0.1)
    isimy <- ranking$ix[isim]
    isimy <- isimy[-which(isimy==poscurr)]
  } else {
    isim <- isimy <- NULL
  }
  
  # prepare table
  stations <- c("BER","SMA","GVE","BAS","ENG","SIO","LUG","SAM")
  
  if (mon<10) {mondate <- paste0("0",mon)} else {mondate <- as.character(mon)}
  begdate <- paste0(ycurr,mondate,"01")
  dpm <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if (ycurr %% 4 == 0) {dpm <- c(31,29,31,30,31,30,31,31,30,31,30,31)}
  enddate <- paste0(ycurr,mondate,dpm[mon])
  
  data <- clim.table::climtable(period=c(begdate,enddate))
  vals <- data$dana$vals
  vals$Region <- rep("",length(vals$Station))
  vals$Region[1:14] <- "Westschweiz"
  vals$Region[15:32] <- "Mittelland"
  vals$Region[33:55] <- "Alpennordhang"
  vals$Region[56:61] <- "Nord- und Mittelbünden"
  vals$Region[62:70] <- "Wallis"
  vals$Region[71:76] <- "Engadin"
  vals$Region[77:88] <- "Alpensüdseite"
  
  acurr_all <- vals$Abw[!is.na(vals$Abw)]
  a_ueber <- length(which(acurr_all > 0.5)) / length(acurr_all)
  a_unter <- length(which(acurr_all < 0.5)) / length(acurr_all)
  a_bereich <- 1 - a_ueber - a_unter
  quac <- quantile(acurr_all,probs = c(0.16,0.84))
  
  # monthly mean temp ranks at stations
  vals$Rank_T <- rep(NA,length(vals$Station))
  vals$firstmeas_T <- rep(NA,length(vals$Station))
  for (s in 1:length(vals$Station)) {
    if (vals$Station[s]=="AND") {
      vals$Rank_T[s] <- NA
      vals$firstmeas_T[s] <- NA
    } else {
      recstat <- rekorde(top=10,minmax="max",year=2024,month=mon,station=vals$Station[s],parameter="ths200m0",rectype="m")
      vals$Rank_T[s] <- recstat$ranks_curryear
      vals$firstmeas_T[s] <- recstat$firstmeas
    }
  }
  
  diff_highlow <- abs(median(vals$Abw[vals$Hoehe>=1500],na.rm=T))-abs(median(vals$Abw[vals$Hoehe<1500],na.rm=T))
  
  # stations with time series of more than 100 years
  ranky100 <- vals$Rank_T[vals$firstmeas_T<(ycurr-100)]
  r1y100 <- which(vals$firstmeas_T<(ycurr-100) & vals$Rank_T==1)
  
  # greatest deviations in all regions
  regs <- unique(vals$Region)
  vhighest <- 1
  vlowest <- 1
  for (r in 1:length(regs)) {
    indr <- which(vals$Region == regs[r])
    vhighest[r] <- quantile(abs(vals$Abw[indr]),0.75,na.rm=TRUE)
    vlowest[r] <- quantile(abs(vals$Abw[indr]),0.25,na.rm=TRUE)
  }
  member_h <- cutree(hclust(dist(vhighest)),3)
  member_l <- cutree(hclust(dist(vlowest)),3)
  mr_h <- 0
  mr_l <- 0
  for (i in 1:3) {
    indm_h <- which(member_h == i)
    mr_h[i] <- mean(vhighest[indm_h])
    indm_l <- which(member_l == i)
    mr_l[i] <- mean(vlowest[indm_l])
  }
  mhigh <- which(mr_h==max(mr_h))
  mhigh <- which(member_h == mhigh)
  regshigh <- collapse_sentence(regs[mhigh])
  mlow <- which(mr_l==min(mr_l))
  mlow <- which(member_l == mlow)
  regslow <- collapse_sentence(regs[mlow])
  # add a few relevant stations to the list below
  selhigh <- vals[which(vals$Region %in% regs[mhigh]),]
  selhigh <- selhigh[order(match(selhigh$Abw, sort(selhigh$Abw,decreasing = TRUE))), ]
  selhigh <- selhigh[1:2,]
  sellow <- vals[which(vals$Region %in% regs[mlow]),]
  sellow <- sellow[order(match(sellow$Abw, sort(sellow$Abw,decreasing = FALSE))), ]
  sellow <- sellow[1:2,]
  selreg <- rbind(selhigh,sellow)
  sellow_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=sellow$Station)$station_name)
  selhigh_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=selhigh$Station)$station_name)
  
  # generate subset for a printable table 
  # (reduced to the stations defined above)
  subset_climtab <- vals[which(vals$Station %in% stations),]
  subset_climtab <- subset_climtab[order(match(subset_climtab$Station, stations)), ]
  subset_climtab <- rbind(subset_climtab,selreg)
  subset_climtab <- subset_climtab[,c(1:5,16:17)]
  sn <- mchdwh::station_info(nat_abbr=subset_climtab$Station)
  sn <- sn[order(match(sn$nat_abbr, subset_climtab$Station)), ]
  subset_climtab$Station <- sn$station_name
  rownames(subset_climtab) <- NULL
  attributes(subset_climtab)$names <- c("Station","Höhe (m)","Monatsmittel (°C)","Norm (°C)","Abweichung (°C)","Rang","Messbeginn")
  
  # # daily records
  # daily_records = day_records(ycurr = ycurr, mon = mon)
  # 
  # numrec_Txx = daily_records$numrec_Txx
  # Txx_sorted_subset = daily_records$Txx_sorted_subset
  # Txx_sorted_subset_pretty = daily_records$Txx_sorted_subset_pretty
  # 
  # numrec_Tnx = daily_records$numrec_Tnx
  # Tnx_sorted_subset = daily_records$Tnx_sorted_subset
  # Tnx_sorted_subset_pretty = daily_records$Tnx_sorted_subset_pretty
  
  # homogoval.eval datenfile für august (abs temp und anonmalie)
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-temp_de.Rmd")
  
  # Add images 
  filename = "monatsbilanz_temp_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_temp(bulletin, valueBase = "abs", provisional = bulletin$provisional, mediaType = "image/png", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
  filename = "monatsbilanz_temp_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_temp(bulletin, valueBase = "anom", provisional = bulletin$provisional, mediaType = "image/png", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
  # Add images 
  filename = "monatsbilanz_temp_map_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "temp", filename = filename),
                        filename = filename,
                        caption = paste0("Monatsmitteltemperaturen in °C für den ",month[mon]," ",ycurr,"."))
  
  filename = "monatsbilanz_temp_map_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "temp", filename = filename),
                        filename = filename,
                        caption = paste0("Abweichungen der Monatsmitteltemperatur von der Norm 1991-2020 in °C für den ",month[mon]," ",ycurr,"."))
  
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
  
  table <- flextable::flextable(df) %>%
    flextable::set_header_labels(values =c("Region", "Mittelwert", "Minimum", "Maximum")) %>%
    flextable::add_header_row(
      values = c("", "Temperaturen"),
      colwidths = c(1,3)
    ) %>%
    flextable::bg(i = ~ as.numeric(TTanom_mean) < 0, j = "TTanom_mean", bg = "#EFEFEF", part = "body") %>%
    flextable::add_footer_lines("Example footer line") %>%
    flextable::set_caption("Regional temperature data") %>%
    flextable::set_table_properties(layout = "autofit")
  table
}

monatsbilanz_precip <- function(bulletin) {

  mon = bulletin$month
  ycurr = bulletin$year
  lang = "G"
  
  provisional = bulletin$provisional
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-precip_de.Rmd")
  
  # Add images 
  filename = "monatsbilanz_prec_map_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "prec", filename = filename),
                        filename = filename,
                        caption = paste0("Monatliche Niederschlagssumme in mm für den ",month[mon]," ",ycurr,"."))
  
  filename = "monatsbilanz_prec_map_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "prec", filename = filename),
                        filename = filename,
                        caption = paste0("Abweichung der monatlichen Niederschlagssumme von der Norm 1991-2020 für den ",month[mon]," ",ycurr,", dargestellt in Prozent der Norm."))
}

monatsbilanz_sun <- function(bulletin) {

  mon = bulletin$month
  ycurr = bulletin$year
  lang = "G"

  provisional = bulletin$provisional
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-sun_de.Rmd")
  
  # Add images 
  filename = "monatsbilanz_sunshine_map_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "sunshine", filename = filename),
                        filename = filename,
                        caption = paste0("Prozent der maximal möglichen Sonnenscheindauer für den ",month[mon]," ",ycurr,"."))
  
  filename = "monatsbilanz_sunshine_map_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "sunshine", filename = filename),
                        filename = filename,
                        caption = paste0("Abweichung der monatlichen Sonnenscheindauer von der Norm 1991-2020 für den ",month[mon]," ",ycurr,", dargestellt in Prozent der Norm."))

}

temporal_evolution <- function(bulletin) {
  
  mon = bulletin$month
  lang <- "G"
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  #filename_abs <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_abs_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
  filename_abs <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "ths200m0.swissmean.m.aug.1864.2024.abs.txt", package = "cat.bulletin")
  data_abs <- read.table(filename_abs, header = TRUE)
  
  filename_anom <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "ths200m0.swissmean.m.aug.1864.2024.anom.txt", package = "cat.bulletin")
  data_anom <- read.table(filename_anom, header = TRUE)
  
  # absolute temperature, swissmean
  year <- data_abs$year
  poscurr <- length(year)
  ycurr <- year[poscurr]
  ybeg <- year[1]
  abs  <- data_abs$val
  
  vcurr <- round(abs[poscurr],1)
  vcurr_t <- format(vcurr, nsmall=1)
  
  # regional rankings    
  ranking <- sort.int(abs,decreasing=T,index.return=T)
  rankcurr <- which(ranking$ix==poscurr)
  
  # loess trend
  loess <- evoclim::loess.filt.knmi(x=abs,years=year,y1=1885,y2=ycurr,y1asmean=TRUE)
  signif <- as.numeric(loess$incr.pval)
  diff <- round(as.numeric(c(loess$conf.l[poscurr]-loess$val1,loess$t.incr,loess$conf.u[poscurr]-loess$val1)),1)
  ydiff_ca <- round(ycurr-1885+1,-1)
  
  resid <- as.numeric(quantile(abs-loess$fit,probs=c(0.16,0.84)))
  
  bounds <- format(round(c(loess$val2+resid[1],loess$val2+resid[2]),1), nsmall=1)
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_temporal-evolution_de.Rmd")
  
}

monatsbulletin_daily_timeseries <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-daily_timeseries_de.Rmd")
  
  filename = "witterungsverlauf.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_witterungsverlauf(bulletin, month=bulletin$month, year=bulletin$year, location="SMA", language="de", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
}

monatsbulletin_more_info <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_more-info_de.Rmd")
}

monatsbulletin_disclaimer <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_disclaimer_de.Rmd")
}

# further helping functions
add_article <- function(word) {
  # Check if the word starts with a vowel (a, e, i, o, u, y)
  if (grepl("^[aeéèiouyAEÉÈIOUY]", word)) {
    return(paste0("d'", tolower(word)))
  } else {
    return(paste0("de ", tolower(word)))
  }
}

ordinal_number <- function(number, gender, lang) {
  # Validate inputs
  if (!is.numeric(number)) stop("Number must be numeric.")
  if (!lang %in% c("G", "F", "I")) stop("Invalid language. Use G for German, F for French, I or Italian.")
  if (!gender %in% c("m", "f")) stop("Invalid gender. Use masculin or female.")
  
  # German case: add a period after the number
  if (lang == "G") {
    return(paste0(number, "."))
  }
  
  # French ordinal logic
  if (lang == "F") {
    if (gender == "m") {
      if (number == 1) return("1er")  # special case for 1st
      return(paste0(number, "e"))
    } else if (gender == "f") {
      if (number == 1) return("1re")  # special case for 1st (female)
      return(paste0(number, "e"))
    }
  }
  
  # Italian ordinal logic
  if (lang == "I") {
    suffix <- ifelse(gender == "m", "o", "a")
    if (number == 1) return(paste0(number, suffix))  # 1st is unique
    return(paste0(number, suffix))
  }
}

collapse_sentence <- function(strings) {
  n <- length(strings)
  
  # Handle different cases based on the number of strings
  if (n == 1) {
    return(strings)  # No need to collapse if there's only one string
  } else if (n == 2) {
    return(paste(strings, collapse = " und "))  # Two strings, collapse with " und "
  } else {
    # More than two strings, collapse with ", " and " und " for the last two
    return(paste(paste(strings[1:(n-1)], collapse = ", "), strings[n], sep = " und "))
  }
}
