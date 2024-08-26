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

fname01 <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".abs.txt")
fname02 <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".anom.txt")
fname01p <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".abs.pdf")
fname02p <- paste0(param,".swissmean.",timespan,".",monname[mon],".1864.",yc,".anom.pdf")
fname_rm <- paste0(param,".swissmean.",timespan,".*.*")

homogval.evol(homog.param = param, 
stations="swissmean", 
begin.date = "186401",
end.date = lastdate,
add.current.mo = acm,
val.resolution = timespan, 
anomalies = FALSE, 
data.line = "loess", 
loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
add.ranking.high = TRUE, 
add.ranking.low = FALSE, 
add.legend = TRUE, 
add.source = TRUE, 
add.2nd.norm.line = c(1991,2020), 
add.2nd.norm.value = TRUE, 
out.path = "current", 
language = "d", 
sig.tool.time = TRUE,
write.txt = TRUE,
axis.cex = 1.2)

homogval.evol(homog.param = param,
stations="swissmean",
begin.date = "186401",
end.date = lastdate,
add.current.mo = acm,
val.resolution = timespan,
anomalies = TRUE,
begin.refperiod = as.character(refper[1]),
end.refperiod = as.character(refper[2]),
data.line = "loess",
loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
add.ranking.high = TRUE,
add.ranking.low = FALSE,
add.legend = TRUE,
add.source = TRUE,
out.path = "current",
language = "d",
sig.tool.time = TRUE,
write.txt = TRUE,
axis.cex = 1.2)

# Remove all unnecessary files
files <- list.files(pattern=fname_rm)
remfiles <- files[!(files %in% c(fname01,fname01p,fname02,fname02p))]
unlink(remfiles)

# Rename pdf for automated report
# HIER WEITERMACHEN

# Parameter settings for calculations and sentences
if (param == "ths200m0") {
	art1 <- c("Die","die")
	art2 <- c("Eine","eine")
	monmean <- "Monatsmitteltemperatur"
	monmean2 <- "landesweit gemittelte Monatstemperatur"
	unit <- "°C"
}

if (mon < 10) { monchar <- paste0(0,mon) } else { monchar <- mon }
month <- c("Januar","Februar","März","April","Mai","Juni",
           "Juli","August","September","Oktober","November","Dezember")
monshort <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
norm <- paste(refper,collapse="-")

filepath01 <- paste0(fname01)
filepath02 <- paste0(fname02)

data01 <- read.table(filepath01,header=T)
year <- data01$year
ycurr <- year[length(year)]
poscurr <- length(year)
ybeg <- year[1]

abs  <- data01$val
vcurr <- round(abs[poscurr],1)
data02 <- read.table(filepath02,header=T)
anom <- data02$val
acurr <- anom[poscurr]
acurr <- round(acurr,1)

# Calculations
ranking <- sort.int(anom,decreasing=T,index.return=T)
rankcurr <- which(ranking$ix==poscurr)

loess <- loess.filt.knmi(x=abs,years=year)
preind <- 1871:1900
ipre <- which(year %in% preind)
mpre <- mean(abs[ipre])
loesscurr <- as.numeric(loess$fit[poscurr])
diff <- round(loesscurr-mpre,1)

X11(width=10,height=6.5)
plot(year,abs,type='s',lwd=2)
lines(year,loess$fit,col='red',lwd=4)

# coldest since:
ind_coldest_since <- which(abs-abs[length(abs)]<=0)
year_cs <- year[ind_coldest_since[length(ind_coldest_since)-1]]
# warmest since:
ind_warmest_since <- which(abs-abs[length(abs)]>=0)
year_ws <- year[ind_warmest_since[length(ind_warmest_since)-1]]


# Paragraph 1: General situation in Switzerland (Swiss mean)
# How warm was the current month?
text01.01 <- paste0(art1[1]," ",monmean," im ",month[mon]," ",ycurr," betrug ",vcurr,"",unit,".")

# How large was the deviation from the current norm period?
text01.02 <- paste0("Dies entspricht einer Abweichung zur Normperiode ",norm," von ",acurr,"",unit,".")

# What was the rank of the current month and since when?
text01.03 <- paste0("Damit belegt der ",month[mon]," ",ycurr," den ",rankcurr,". Rang seit Messbeginn ",ybeg,".")

# Which was the warmest month so far?
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

if (rankcurr == 1) {
	subtitle <- paste0("Wärmster ",month[mon]," seit Messbeginn 1864")
} else if (rankcurr < 6) {
        subtitle <- paste0(rankcurr,".-wärmster ",month[mon]," seit Messbeginn 1864")
} else if (acurr < -3) {
	subtitle <- paste0("Temperatur im ",month[mon]," weit unter der Norm")
} else if (acurr < -1) {
	subtitle <- paste0("Unterdurchschnittliche Temperatur im ",month[mon])
} else if (acurr >= -1 & acurr <= 1) {
	subtitle <- paste0("Temperaturen im ",month[mon]," im Normbereich")
} else if (acurr > 3) {
	subtitle <- paste0("Temperatur im ",month[mon]," weit über der Norm")
} else if (acurr > 1) {
        subtitle <- paste0("Überdurchschnittliche Temperatur im ",month[mon])
}

text01.05 <- paste0("Der damalige ",month[mon]," erreichte ",art2[2]," ",monmean," von ",recval,"",unit,", bei einer Abweichung von ",reca,"",unit," zur Norm ",norm,".")

# Have temperatures increased significantly in this month since pre-industrial levels? And by how much?
if (diff > 0) {
	text01.06 <- paste0("Im Vergleich zur vorindustriellen Referenzperiode 1871-1900 ist ",art1[2]," ",monmean2," im ",month[mon]," um ",diff,"",unit," gestiegen.")
} else {
	text01.06 <- paste0("Im Vergleich zur vorindustriellen Referenzperiode 1871-1900 ist ",art1[2]," ",monmean2," im ",month[mon]," um ",diff,"",unit," gesunken.")
}

# Print all text
# Paragraph 1
paragraph01 <- paste(text01.01,text01.02,text01.03,text01.04,text01.05,text01.06,sep=" ")
paragraph01 <- gsub("  "," ",paragraph01)
print(paragraph01)

sink("outfile.tex")
cat(paragraph01)
sink()

sink("subtitle_temp.tex")
cat(subtitle)
sink()

}
