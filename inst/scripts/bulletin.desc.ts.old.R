bulletin.desc.ts <- function(par,ref_period,enddate,timespan) {

library(evoclim)
library(mchdwh)

lastdate <- substr(enddate,1,6)
yc <- substr(enddate,1,4)
mon <- as.numeric(substr(enddate,5,6))

if (par == "T") {
	param <- "ths200m0"
} else if (param == "P") {
	param <- "rhs150m0"
}

refper <- ref_period

# if current month then end.date = NULL and add.current.mo = TRUE, 
# else end.date = "202402" and add.current.mo = FALSE
if (lastdate == format(Sys.Date(),"%Y%m")) {
	lastdate <- NULL
	acm <- TRUE
} else {
	acm <- FALSE
}

monname <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
regnames <- c("südlich des Alpenhauptkamms","auf der Alpennordseite unter 1000 Meter","nördlich der Alpen über 1000 Meter")

if (par == "T") {

  fname01 <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".abs.txt")
  fname02 <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".anom.txt")
  fname03 <- paste0(param,".swissmean_south.",timespan,".",monname[mon],".1864.",yc,".anom.txt")
  fname04 <- paste0(param,".swissmean_north_low.",timespan,".",monname[mon],".1864.",yc,".anom.txt")
  fname05 <- paste0(param,".swissmean_north_high.",timespan,".",monname[mon],".1864.",yc,".anom.txt")
  fname01p <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".abs.pdf")
  fname02p <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".anom.pdf")
  fname03p <- paste0(param,".swissmean_south.",timespan,".",monname[mon],".1864.",yc,".anom.pdf")
  fname04p <- paste0(param,".swissmean_north_low.",timespan,".",monname[mon],".1864.",yc,".anom.pdf")
  fname05p <- paste0(param,".swissmean_north_high.",timespan,".",monname[mon],".1864.",yc,".anom.pdf")
  fname_rm <- paste0(param,".swissmean|swissmean_south|swissmean_north_low|swissmean_north_high.",timespan,".*.*")

  # swissmean abs
  homogval.evol(homog.param = param, stations="swissmean", begin.date = "186401", end.date = lastdate,
                add.current.mo = acm, val.resolution = timespan, anomalies = FALSE, data.line = "loess", 
                loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
                add.ranking.high = TRUE, add.ranking.low = FALSE, add.legend = TRUE, add.source = TRUE, 
                add.2nd.norm.line = c(1991,2020), add.2nd.norm.value = TRUE, out.path = "current", language = "d", 
                sig.tool.time = TRUE, write.txt = TRUE, axis.cex = 1.2)

  #swissmean anom
  homogval.evol(homog.param = param, stations="swissmean", begin.date = "186401", end.date = lastdate,
                add.current.mo = acm, val.resolution = timespan, anomalies = TRUE, begin.refperiod = as.character(refper[1]),
                end.refperiod = as.character(refper[2]), data.line = "loess", 
                loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
                add.ranking.high = TRUE, add.ranking.low = FALSE, add.legend = TRUE, add.source = TRUE, out.path = "current",
                language = "d", sig.tool.time = TRUE, write.txt = TRUE, axis.cex = 1.2)

  # swissmean south, anom
  homogval.evol(homog.param = param, stations="swissmean.south", begin.date = "186401", end.date = lastdate, 
                add.current.mo = acm, val.resolution = timespan, anomalies = TRUE, begin.refperiod = as.character(refper[1]),
                end.refperiod = as.character(refper[2]), data.line = "loess",
                loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
                add.ranking.high = TRUE, add.ranking.low = FALSE, add.legend = TRUE, add.source = TRUE,
                out.path = "current", language = "d", sig.tool.time = TRUE, write.txt = TRUE, axis.cex = 1.2)

  # swissmean north low, anom
  homogval.evol(homog.param = param, stations="swissmean.north.low", begin.date = "186401", end.date = lastdate,
                add.current.mo = acm, val.resolution = timespan, anomalies = TRUE, begin.refperiod = as.character(refper[1]),
                end.refperiod = as.character(refper[2]), data.line = "loess", 
                loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
                add.ranking.high = TRUE, add.ranking.low = FALSE, add.legend = TRUE, add.source = TRUE, out.path = "current",
                language = "d", sig.tool.time = TRUE, write.txt = TRUE, axis.cex = 1.2)

  # swissmean north high, anom
  homogval.evol(homog.param = param, stations="swissmean.north.high", begin.date = "186401", end.date = lastdate, add.current.mo = acm,
                val.resolution = timespan, anomalies = TRUE, begin.refperiod = as.character(refper[1]), 
                end.refperiod = as.character(refper[2]), data.line = "loess", 
                loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
                add.ranking.high = TRUE, add.ranking.low = FALSE, add.legend = TRUE, add.source = TRUE, out.path = "current",
                language = "d", sig.tool.time = TRUE, write.txt = TRUE, axis.cex = 1.2)

  # Remove all unnecessary files
  files <- list.files(pattern=fname_rm)
  remfiles <- files[!(files %in% c(fname01,fname01p,fname02,fname02p,fname03,fname03p,fname04,fname04p,fname05,fname05p))]
  unlink(remfiles)

  if (mon < 10) { monchar <- paste0(0,mon) } else { monchar <- mon }
  month <- c("Januar","Februar","März","April","Mai","Juni",
            "Juli","August","September","Oktober","November","Dezember")
  monshort <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  norm <- paste(refper,collapse="-")

  filepath_abs <- paste0(fname01)
  filepath_anom <- paste0(fname02)
  filepaths_reganom <- c(paste0(fname03), paste0(fname04), paste0(fname05))

  # absolute temperature, swissmean
  data_abs <- read.table(filepath_abs, header=T)
  year <- data_abs$year
  ycurr <- year[length(year)]
  poscurr <- length(year)
  ybeg <- year[1]
  abs  <- data_abs$val
  vcurr <- round(abs[poscurr],1)

  data_anom <- read.table(filepath_anom, header=T)
  anom <- data_anom$val
  acurr <- round(anom[poscurr],1)
  
  # Regional rankings    
  ranking <- sort.int(anom,decreasing=T,index.return=T)
  rankcurr <- which(ranking$ix==poscurr)

  loess <- loess.filt.knmi(x=abs,years=year)
  preind <- 1871:1900
  ipre <- which(year %in% preind)
  mpre <- mean(abs[ipre])
  loesscurr <- as.numeric(loess$fit[poscurr])
  diff <- round(loesscurr-mpre,1)
  
  # Paragraph 1: General situation in Switzerland (Swiss mean)
  # How warm was the current month?
  text01.01 <- paste0("Die landesweit gemittelte Monatstemperatur im ",month[mon]," ",ycurr," betrug ",vcurr,"°C.")
  
  # How large was the deviation from the current norm period?
  text01.02 <- paste0("Dies entspricht einer Abweichung zur Normperiode ",norm," von ",acurr,"°C.")
  
  # What was the rank of the current month and since when?
  text01.03 <- paste0("Damit belegt der ",month[mon]," ",ycurr," im Schweizer Durchschnitt den ",rankcurr,". Rang seit Messbeginn ",ybeg,".")
  
  # Warmest month so far
  # If the current month is the warmest (new record), give year and values of rank 2, otherwise take rank 1
  if (rankcurr != 1) {
    ind01 <- ranking$ix[1]
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
    text01.04 <- paste0("Der bisher wärmste ",month[mon]," stammt aus dem Jahr ",recy,".")
  } else {
    ind01 <- ranking$ix[2]
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
    text01.04 <- paste0("Der bisher wärmste ",month[mon]," stammte aus dem Jahr ",recy,".")
  }
  text01.05 <- paste0("Der damalige ",month[mon]," erreichte eine Monatsmitteltemperatur von ",
                      recval,"°C, bei einer Abweichung von ",reca,"°C zur Norm ",norm,".")

  # Have temperatures increased significantly in this month since pre-industrial levels? And by how much?
  if (diff > 0) {
    text01.06 <- paste0("Im Vergleich zur vorindustriellen Referenzperiode 1871-1900 ist die landesweit gemittelte Monatstemperatur im ",month[mon]," um ",diff,"°C gestiegen.")
  } else {
    text01.06 <- paste0("Im Vergleich zur vorindustriellen Referenzperiode 1871-1900 ist die landesweit gemittelte Monatstemperatur im ",month[mon]," um ",diff,"°C gesunken.")
  }
  
  # Regional anomalies
  acurr_reg <- 0
  rankcurr_reg <- 0
  anom_reg <- array(NA, c(length(year),length(filepaths_reganom)))
  for (i in 1:length(filepaths_reganom)) {
    data <- read.table(filepaths_reganom[i],header=T)
    anom_reg[,i] <- data$val
    acurr_reg[i] <- round(anom_reg[poscurr,i],1)

    # Regional rankings    
    ranking <- sort.int(anom_reg[,i],decreasing=T,index.return=T)
    rankcurr_reg[i] <- which(ranking$ix==poscurr)
  }
  
  # Where the ranks equal in all subregions?
  # Case 1: all ranks the same
  if (all(rankcurr_reg==rankcurr)) {
    if (!all(acurr_reg==acurr_reg[1])) {
      if (acurr_reg[3] == acurr_reg[2]) {
        text01.07 <- paste0("Nördlich der Alpen wurde im Durchschnitt eine Abweichung von ",acurr_reg[2],"°C zur Norm registriert.")
      } else {
        text01.07 <- paste0("Nördlich der Alpen wurde in tieferen Lagen durchschnittlich eine Abweichung von ",acurr_reg[2],"°C zur Norm registriert. In hohen Lagen lag die Abweichung des Monatsmittel bei ",acurr_reg[3],"°C.")
       }
       text01.08 <- paste0("Die Südschweiz verzeichnete eine Abweichung der Monatsmitteltemperatur von ",acurr_reg[1],"°C zur Norm.")
     } else {
       text01.07 <- ""
       text01.08 <- ""
     }
   } else {
     rkr <- sort.int(rankcurr_reg,decreasing = F,index.return = T)
     # Case 2: two same, one lower rank
     # Hier noch die Abweichungen in °C erwähnen.
     if (rkr$x[1]==rkr$x[2] & rkr$x[3]>rkr$x[1]) {
       text01.07.1 <- paste0(regnames[rkr$ix[1]]," und ",regnames[rkr$ix[2]]," belegte der ",month[mon]," ",ycurr," den ",rkr$x[1],". Rang seit Messbeginn.")
       text01.07.1 <- paste0(toupper(substr(text01.07.1, 1, 1)),substr(text01.07.1,2,nchar(text01.07.1)))
       text01.07.2 <- paste0(regnames[rkr$ix[3]]," war es Rang ",rkr$x[3],".")
       text01.07.2 <- paste0(toupper(substr(text01.07.2, 1, 1)),substr(text01.07.2,2,nchar(text01.07.2)))
       text01.07 <- paste(text01.07.1,text01.07.2,collapse = " ")
     }
     # Case 3: two same, one higher rank
     if (rkr$x[2]==rkr$x[3] & rkr$x[1]<rkr$x[2]) {
       text01.07.1 <- paste0(regnames[rkr$ix[1]]," lag die Monatsmitteltemperatur im ",month[mon]," ",ycurr," auf dem ",rkr$x[1],". Rang seit Messbeginn.")
       text01.07.1 <- paste0(toupper(substr(text01.07.1, 1, 1)),substr(text01.07.1,2,nchar(text01.07.1)))
       text01.07.2 <- paste0(regnames[rkr$ix[2]]," und ",regnames[rkr$ix[3]]," gab es Rang ",rkr$x[2],".")
       text01.07.2 <- paste0(toupper(substr(text01.07.2, 1, 1)),substr(text01.07.2,2,nchar(text01.07.2)))
       text01.07 <- paste(text01.07.1,text01.07.2,collapse = " ")
     }
     # Case 4: all different
     if (length(rkr$x)==length(unique(rkr$x))) {
       text01.07 <- paste0("Rang ",rkr$x[1]," belegte der ",month[mon]," ",ycurr," ",regnames[rkr$ix[1]],". ",regnames[rkr$ix[2]]," gab es Rang ",rkr$x[2],", ",regnames[rkr$ix[3]]," Rang ",rkr$x[3],".")
       text01.07 <- paste0(toupper(substr(text01.07, 1, 1)),substr(text01.07,2,nchar(text01.07)))
     }
  }

  # Print all text
  # Paragraph 1
  paragraph01 <- paste(text01.01,text01.02,text01.03,text01.04,text01.05,text01.06,text01.07,sep=" ")
  paragraph01 <- gsub("  "," ",paragraph01)
  print(paragraph01)

  sink("outfile.tex")
  cat(paragraph01)
  sink()

}

}
