#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function() {
  
  bulletin <- create_bulletin() %>%
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
    add_text(bulletin, paste("# Klimabulletin", Sys.Date())) %>%
    add_text(paste("Im Leadtext Reihenfolge der zu nennenden Parameter über die Ränge entscheiden. Super wären Sätze im Sinne von DER AUGUST 2024 WAR GEPRÄGT VON HOHEN TEMPERATUREN UND WENIG NIEDERSCHLAG.")) %>%
    add_image(filepath = system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png"),
              filename = "loess.png",
              caption = "This is a caption.")
}

monatsbilanz_temp <- function(bulletin) {
  
  #input aus anaperiod
  mon <- 8

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
  
  # provisorisch: climate-evolution-series-outlook monthly daten file
  # Absolutwerte:
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-evolution-series-outlook?cg1-static.valueBase=abs&cg1-static.timeGranularity=month&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution-outlook&lang=de
  # Anomalie:
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-evolution-series-outlook?cg1-static.valueBase=anom&cg1-static.timeGranularity=month&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution-outlook&lang=de 
  
  # Definitiv  für entsprechenden Monat: climate-temperature-evolution
  # https://service.meteoswiss.ch/productbrowser/authenticated/productDisplay/climate-temperature-evolution?cg1-static.valueBase=abs&cg1-static.timeOfYear=08&cg1-static.normalPeriod=1991-2020&cg1-static.location=regSwiss&cg1-static.language=de&cg1-static.plotPeriod=1864-today&cg1-static.productName=climate-temperature-evolution&lang=de
  
  # Tables:
  # https://rmarkdown.rstudio.com/lesson-7.html, knitr::kable?
  # Anforderungen für Tabellen definieren, nicht alles in markdown lösen wegen xml
  # Bei der Tabelle sicherstellen, dass Abweichungen mit den entsprechenden Farbskalen der Kartengrafik eingefärbt sind.
  
  filename_abs <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "ths200m0.swissmean.m.aug.1864.2024.abs.txt", package = "cat.bulletin")
  data_abs <- read.table(filename_abs, header = TRUE)

  filename_anom <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "ths200m0.swissmean.m.aug.1864.2024.anom.txt", package = "cat.bulletin")
  data_anom <- read.table(filename_anom, header = TRUE)
  
#  filename_anom_south <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "ths200m0.swissmean_south.m.aug.1864.2024.anom.txt", package = "cat.bulletin")
#  data_anom_south <- read.table(filename_anom_south, header = TRUE)

#  filename_anom_north_low <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "ths200m0.swissmean_north_low.m.aug.1864.2024.anom.txt", package = "cat.bulletin")
#  data_anom_north_low <- read.table(filename_anom_north_low, header = TRUE)

#  filename_anom_north_high <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "ths200m0.swissmean_north_high.m.aug.1864.2024.anom.txt", package = "cat.bulletin")
#  data_anom_north_high <- read.table(filename_anom_north_high, header = TRUE)
  
  
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

#  acurr_reg <- rep(NA,3)
#  acurr_reg[1] <- round(data_anom_north_low$val[poscurr],1)
#  acurr_reg[2] <- round(data_anom_north_high$val[poscurr],1)
#  acurr_reg[3] <- round(data_anom_south$val[poscurr],1)
  
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
  reca_t <- format(reca, nsmall=1)
  recval_t <- format(recval, nsmall=1)
  
  # years similar to current
  diffc_t5 <- abs(ranking$x[which(ranking$ix==poscurr)]-ranking$x[1:5])
  if (any(diffc_t5<0.1)) {
    isim <- which(diffc_t5<0.1)
    isimy <- ranking$ix[isim]
    isimy <- isimy[-which(isimy==poscurr)]
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
  a_ueber <- length(which(acurr_all>0.5))/length(acurr_all)
  a_unter <- length(which(acurr_all<0.5))/length(acurr_all)
  a_bereich <- 1-ueber-unter
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
  
  # Tagesrekorde
  dayrec_stats <- c("ABO","AIG","ALT","ARO","BAS","BEH","BER","BLA","BRL","BUF","BUS","CDF","CGI","CHA","CHD","CHM","CHU",
                    "CIM","COM","COV","DAV","DEM","DIS","DOL","EBK","EIN","ELM","ENG","EVO","FAH","FRE","GLA","GRA","GRC",
                    "GRH","GRO","GSB","GUE","GUT","GVE","HAI","HLL","HOE","INT","JUN","KLO","KOP","LAG","LUG","LUZ","MAG",
                    "MER","MLS","MVE","NAP","NEU","OTL","PAY","PIL","PIO","PLF","PUY","RAG","REH","ROB","ROE","RUE","SAE",
                    "SAM","SBE","SBO","SCU","SHA","SIA","SIO","SMA","SMM","STG","TAE","ULR","VAD","VIS","WAE","WFJ","WYN","ZER")
  Txx <- rep(NA,length(dayrec_stats))
  Rank_Txx <- rep(NA,length(dayrec_stats))
  firstmeas_Txx <- rep(NA,length(dayrec_stats))
  date_Txx <- rep(NA,length(dayrec_stats))
  for (s in 1:length(dayrec_stats)) {
    recstat <- rekorde(top=10,minmax="max",year=ycurr,month=mon,station=dayrec_stats[s],parameter="ths200dx",rectype="m")
    Rank_Txx[s] <- recstat$ranks_curryear[1]
    Txx[s] <- recstat$values_curryear[1]
    firstmeas_Txx[s] <- recstat$firstmeas
    date_Txx[s] <- paste0(substr(recstat$dates_curryear[1],7,8),".",substr(recstat$dates_curryear[1],5,6),".")
  }
  Txx_all <- data.frame(Txx,date_Txx,Rank_Txx,firstmeas_Txx)
  row.names(Txx_all) <- dayrec_stats
  Txx_sorted <- Txx_all[order(Txx_all$Rank_Txx), ]
  Txx_sorted_subset <- Txx_sorted[Txx_sorted$Rank_Txx <= 5 & Txx_sorted$firstmeas_Txx <= 1959, ]
  # number of new Txx records
  numrec_Txx <- dim(Txx_sorted_subset)[1]
  sinf_Txx <- mchdwh::station_info(nat_abbr=row.names(Txx_sorted_subset))
  row.names(Txx_sorted_subset) <- sinf_Txx$station_name[order(match(sinf_Txx$nat_abbr,row.names(Txx_sorted_subset)))]
  Txx_sorted_subset_pretty <- Txx_sorted_subset
  names(Txx_sorted_subset_pretty) <- c("Maximale Temperatur (°C)","Datum","Rang","Messbeginn")
  if (numrec_Txx>6) {
    Txx_sorted_subset_pretty <- Txx_sorted_subset_pretty[1:6,]
  }
  
  Tnx <- rep(NA,length(dayrec_stats))
  Rank_Tnx <- rep(NA,length(dayrec_stats))
  firstmeas_Tnx <- rep(NA,length(dayrec_stats))
  date_Tnx <- rep(NA,length(dayrec_stats))
  for (s in 1:length(dayrec_stats)) {
    recstat <- rekorde(top=10,minmax="max",year=ycurr,month=mon,station=dayrec_stats[s],parameter="ths200dn",rectype="m")
    Rank_Tnx[s] <- recstat$ranks_curryear[1]
    Tnx[s] <- recstat$values_curryear[1]
    firstmeas_Tnx[s] <- recstat$firstmeas
    date_Tnx[s] <- paste0(substr(recstat$dates_curryear[1],7,8),".",substr(recstat$dates_curryear[1],5,6),".")
  }
  Tnx_all <- data.frame(Tnx,date_Tnx,Rank_Tnx,firstmeas_Tnx)
  row.names(Tnx_all) <- dayrec_stats
  Tnx_sorted <- Tnx_all[order(Tnx_all$Rank_Tnx), ]
  Tnx_sorted_subset <- Tnx_sorted[Tnx_sorted$Rank_Tnx <= 5 & Tnx_sorted$firstmeas_Tnx <= 1959, ]
  # number of new Txx records
  numrec_Tnx <- dim(Tnx_sorted_subset)[1]
  sinf_Tnx <- mchdwh::station_info(nat_abbr=row.names(Tnx_sorted_subset))
  row.names(Tnx_sorted_subset) <- sinf_Tnx$station_name[order(match(sinf_Tnx$nat_abbr,row.names(Tnx_sorted_subset)))]
  Tnx_sorted_subset_pretty <- Tnx_sorted_subset
  names(Tnx_sorted_subset_pretty) <- c("Höchstes Tagesminimum (°C)","Datum","Rang","Messbeginn")
  if (numrec_Tnx>6) {
    Tnx_sorted_subset_pretty <- Tnx_sorted_subset_pretty[1:6,]
  }
  
  # statslow_north <- vals$Station[vals$Region %in% c("Westschweiz","Mittelland","Alpennordhang","Wallis") & vals$Hoehe < 500]
  # lownorth_Rank_Tmax <- rep(NA, length(statslow_north))
  # lownorth_Tmax <- rep(NA, length(statslow_north))
  # for (s in 1:length(statslow_north)) {
  #   recstat <- rekorde(top=10,minmax="max",year=2024,month=mon,station=statslow_north[s],parameter="ths200dx",rectype="m")
  #   lownorth_Rank_Tmax[s] <- recstat$ranks_curryear[1]
  #   lownorth_Tmax[s] <- recstat$values_curryear[1]
  # }
  # statslow_south <- vals$Station[vals$Region == "Alpensüdseite" & vals$Hoehe < 400]
  # lowsouth_Rank_Tmax <- rep(NA, length(statslow_south))
  # lowsouth_Tmax <- rep(NA, length(statslow_south))
  # for (s in 1:length(statslow_south)) {
  #   recstat <- rekorde(top=10,minmax="max",year=2024,month=mon,station=statslow_south[s],parameter="ths200dx",rectype="m")
  #   lowsouth_Rank_Tmax[s] <- recstat$ranks_curryear[1]
  #   lowsouth_Tmax[s] <- recstat$values_curryear[1]
  # }
  
  
  # homogoval.eval datenfile für august (abs temp und anonmalie)
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-temp_de.Rmd")

}

monatsbilanz_precip <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-precip_de.Rmd")
}

monatsbilanz_sun <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-sun_de.Rmd")
}

temporal_evolution <- function(bulletin) {
  
  lang <- "G"
  
  #input aus anaperiod
  mon = 8

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

rekorde <- function(top=10,minmax="max",year=2024,month=2,station="SMA",parameter="ths200m0",rectype="m") {
  # Get records for different variables up to current month
  # Input
  if (rectype=="m") {
    data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                            param_short=parameter,
                                            year=c(1864,year),
                                            month=month,meas_cat=1),
                     error=function(e) e)
    if (is(data,"error")) {
      data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                              param_short=parameter,
                                              year=c(1864,year),
                                              month=month,meas_cat=12),
                       error=function(e) e)
    }
  }
  if (rectype=="y") {
    data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                            param_short=parameter,
                                            year=c(1864,year),meas_cat=1),
                     error=function(e) e)
    if (is(data,"error")) {
      data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                              param_short=parameter,
                                              year=c(1864,year),meas_cat=12),
                       error=function(e) e)
    }
  }
  
  
  if (minmax == "max") {
    dsort <- order(data$value,as.numeric(data$datetime),decreasing=T)
  } else {
    dsort <- order(data$value,as.numeric(data$datetime),decreasing=F)
  }
  vals <- data$value[dsort]
  ranks <- 1
  for (i in 2:length(vals)) {
    if (vals[i]-vals[i-1] != 0) {
      ranks[i] <- i
    } else {
      ranks[i] <- ranks[i-1]
    }
  }
  
  ind_recs <- which(ranks <= top)
  if (rectype=="m") {
    ind_curr <- which(dsort %in% which(as.numeric(substr(data$datetime,1,4))==year))
  } else {
    ind_curr <- which(dsort %in% which(as.numeric(substr(data$datetime,1,4))==year & as.numeric(substr(data$datetime,5,6))==month))
  }
  
  record <- list()
  record$datetime <- data$datetime[dsort][ind_recs]
  record$value <- data$value[dsort][ind_recs]
  record$ranks <- ranks[ind_recs]
  record$firstmeas <- as.numeric(substr(data$datetime[1],1,4))
  
  if (length(ind_curr)>=1) {
    ranks_currentyear <- ranks[ind_curr]
    dates_currentyear <- data$datetime[dsort][ind_curr]
    values_currentyear <- data$value[dsort][ind_curr]
    record$dates_curryear <- dates_currentyear
    record$values_curryear <- values_currentyear
    record$ranks_curryear <- ranks_currentyear
  }
  
  return(record)
  
}