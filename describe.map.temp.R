library(ncdf4)
library(gridmch)
library(geocors)
library(mchdwh)
library(colkd)
library(clim.table)

source('period.to.analyse.R')

load('prec.masks.CH.regions.rda')
regions <- prec.masks.CH.regions

regions_fall1 <- c("Der Jura","Das westliche Mittelland","Das östliche Mittelland","Der westliche Alpennordhang",
             "Der östliche Alpennordhang","Das Wallis","Die Region Graubünden und Engadin","Die Alpensüdseite")
regions_fall2 <- c("im Jura","im westlichen Mittelland","im östlichen Mittelland","am westlichen Alpennordhang",
             "am östlichen Alpennordhang","im Wallis","im Kanton Graubünden und Engadin","auf der Alpensüdseite")

enddate <- gsub("-",".",as.character(Sys.Date()-1))
begdate <- gsub("-",".",paste0(substr(Sys.Date(),1,8),"01"))

year <- as.numeric(substr(begdate,1,4))
mon  <- as.numeric(substr(begdate,6,7))
yearmon <- paste0(substr(begdate,1,4),substr(begdate,6,7))

plot.title <- ""
breaks <- c(-7,-6,-5,-4,-3,-2,-1.5,-1,-0.5,0.5,1,1.5,2,3,4,5,6,7)
file.name.prefix <- "temperatur_monat_"

stats <- c("BER","SMA","GVE","CHD","BAS","ENG","DAV","JUN","SIO","LUG","OTL","SAM","GSB","STG","GRC")

# Entscheid ob Vormonatswerte zu nehmen sind oder laufender Monat (wenn Datum > 21)
if (as.numeric(substr(enddate,9,10)) >= 21) {
# Laufender Monat: Mittel rechnen fuer bisherige Tage
product <- "TanomD9120"
t.beg <- begdate
t.end <- enddate
t.format <- "yyyy.mm.dd"
plot.title <- ""
breaks <- c(-7,-6,-5,-4,-3,-2,-1.5,-1,-0.5,0.5,1,1.5,2,3,4,5,6,7)
file.name.prefix <- "temperatur_monat_"

# Berechnungs-Teil (hier nicht aendern)
grd <- gridmch(pname=product,
            t.beg=t.beg,t.end=t.end,t.format=t.format,
            grid.name="ch01r.swiss.lv95",do.plot=FALSE,return.what="grid")
agg <- apply(grd,FUN=mean,MARGIN=c(1,2))
agg <- copy.grid.atts(from=grd,to=agg)
attr(agg,"time") <- attr(grd,"time")[1]
attr(agg,"grid.name") <- attr(grd,"grid.name")

# Graphik-Teil (hier nicht aendern)
plot.spec <- plot.specs(appear="Intranet", pname=product, 
                        user.opts=list(plot.stats=FALSE,breaks=breaks,
                                       main.pre=plot.title,main.mid="",
                                       fname.plot=file.name.prefix,
                                       method.label="",date.label="date"))
do.call(what = "gridmch.plot", 
        args = c(list(grid=agg,dat=NULL,pname=product,gr.format="pdf"),plot.spec))

vdata <- agg

t.beg.climtab <- gsub(pattern="\\.",replace="",as.character(begdate))
t.end.climtab <- gsub(pattern="\\.",replace="",as.character(enddate))

climtab <- climtable(period=c(t.beg.climtab,t.end.climtab))
climtab <- climtab$dana

statval <- 0
ind_am <- 0
sname <- ""
c <- 0
for (i in stats) {
  c <- c+1
  ist <- which(climtab$vals$Station == i)
  statval[c] <- climtab$vals$Abw[ist]
  tt <- dwhget_surface(nat_abbr = i,
                 param_short = c("th9120mv"),
                 year=c(1864:year),meas_cat=1)
  tt$value[length(tt$value)] <- climtab$vals$Abw[ist]
  allm <- tt$value[which(substr(tt$datetime,5,6)==substr(yearmon,5,6))]
  ya <- as.numeric(substr(tt$datetime[which(substr(tt$datetime,5,6)==substr(yearmon,5,6))],1,4))
  names(allm) <- ya
  allm <- sort(allm,decreasing=T)
  ind_am[c] <- min(which(allm==statval[c]))
  sname[c] <- station_info(nat_abbr=i)$station_name
}
names(statval) <- sname

} else {
#Letzter Monat laden und plot zeichnen
library(lubridate)
ym1 <- format(Sys.Date() %m-% months(1),"%Y.%m")

year <- as.numeric(substr(ym1,1,4))
mon  <- as.numeric(substr(ym1,6,7))
yearmon <- paste0(substr(ym1,1,4),substr(ym1,6,7))

t.beg.climtab <- gsub("-","",floor_date(Sys.Date() %m-% months(1),"month"))
t.end.climtab <- gsub("-","",ceiling_date(Sys.Date() %m-% months(1),"month") %m-% days(1))

climtab <- climtable(period=c(t.beg.climtab,t.end.climtab))
climtab <- climtab$dana

m.beg <- paste0(substr(yearmon,1,4),".",substr(yearmon,5,6))
m.end <- m.beg
t.format <- "yyyy.mm"

# Berechnungs-Teil (hier nicht aendern)
product <- "TanomM9120"
grd <- gridmch(pname=product,
            t.beg=m.beg,t.end=m.end,t.format=t.format,
            grid.name="ch01r.swiss.lv95",do.plot=FALSE,return.what="grid")

# Graphik-Teil (hier nicht aendern)
plot.spec <- plot.specs(appear="Intranet", pname=product,
                        user.opts=list(plot.stats=FALSE,breaks=breaks,
                                       main.pre=plot.title,main.mid="",
                                       fname.plot=file.name.prefix,
                                       method.label="",date.label="date"))
do.call(what = "gridmch.plot",
        args = c(list(grid=grd,dat=NULL,pname=product,gr.format="pdf"),plot.spec))

vdata <- grd

# Für einzelne Beispielstationen pro Region Werte aus DWH laden
statval <- 0
ind_am <- 0
sname <- ""
c <- 0
for (i in stats) {
  c <- c+1
  ist <- which(climtab$vals$Station == i)
  statval[c] <- climtab$vals$Abw[ist]
  tt <- dwhget_surface(nat_abbr = i,
                 param_short = c("th9120mv"),
                 year=c(1864:year),meas_cat=1)
  tt$value[length(tt$value)] <- climtab$vals$Abw[ist]
  allm <- tt$value[which(substr(tt$datetime,5,6)==substr(yearmon,5,6))]
  ya <- as.numeric(substr(tt$datetime[which(substr(tt$datetime,5,6)==substr(yearmon,5,6))],1,4))
  names(allm) <- ya
  allm <- sort(allm,decreasing=T)
  ind_am[c] <- min(which(allm==statval[c]))
  sname[c] <- station_info(nat_abbr=i)$station_name
}
names(statval) <- sname

}

# Station plot
rg <- range(statval)
rgn <- 0
if (prod(rg)>0) {
        rgn[1] <- 0
        rgn[2] <- abs(max(rg))
} else {
        rgn <- rg
}
df <- abs(diff(rgn))/100

cols <-colkd.temp.anom()
statcol <- cut(sort(statval),breaks=c(-10000,breaks,10000),labels=F)

pdf("temp_dev_stats.pdf",width=10,height=8)
par(mar=c(5,17,1,1))
bp <- barplot(sort(statval),horiz=T,las=1,xlab="Abweichungen zur Norm 1991-2020 (°C)",
              col=cols[statcol],cex.axis=1.5,cex.names=1.5,cex.lab=1.5)
text(x=sort(statval)-df,y=bp,paste0(sort(statval)," (",ind_am[sort.int(statval,index.return=T)$ix],")"),adj=1,cex=1.5)
dev.off()

# Map analysis
month <- c("Januar","Februar","März","April","Mai","Juni",
	   "Juli","August","September","Oktober","November","Dezember")

regions_fall1 <- c("Der Jura","Das westliche Mittelland","Das östliche Mittelland","Der westliche Alpennordhang",
             "Der östliche Alpennordhang","Das Wallis","Die Region Graubünden und Engadin","Die Alpensüdseite")
regions_fall2 <- c("im Jura","im westlichen Mittelland","im östlichen Mittelland","am westlichen Alpennordhang",
             "am östlichen Alpennordhang","im Wallis","im Kanton Graubünden und Engadin","auf der Alpensüdseite")

#######################################
# Was it too warm or too cold in general?
# What are the largest and the lowest values in the map?
allv <- vdata[!is.na(vdata)]

qa <- round(quantile(allv,prob=c(0,0.2,0.8,1)),1)

text01.01 <- ""
if (min(allv) > 0.5) {
	text01.01 <- paste0("Die Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 waren im ",month[mon]," schweizweit über dem Durchschnitt.")
} else if (max(allv) < -0.5) {
	text01.01 <- paste0("Die Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 waren im ",month[mon]," schweizweit unter dem Durchschnitt.")
}
text01.02 <- paste0("Die Anomalien der ",month[mon],"temperatur lagen verbreitet zwischen ",qa[2],"°C und ",qa[3],"°C.")
text01.03 <- paste0("Lokal wurden auch niedrigere Abweichungen von bis zu ",qa[1],"°C und höhere Werte bis ",qa[4],"°C registriert.")


#########################################
# Regional differences
rabbr <- attributes(regions)$names
qs <- seq(0,1,0.01)
qr <- array(NA,c(length(rabbr),length(qs)))
qall <- quantile(vdata,prob=qs,na.rm=T)
sqr <- 0
for (i in 1:length(rabbr)) {
        indr <- which(regions[[i]] == 1)
        qr[i,] <- quantile(vdata[indr],prob=qs)
        sqr[i] <- sum(qr[i,]-qall)
}

# Regionen mit höchsten und tiefsten Werten im Mittel
si1 <- sort.int(sqr,decreasing=T,index.return=T)
text01.04 <- paste0(regions_fall1[si1$ix[1]]," wies insgesamt höhere Werte aus als andere Regionen. Die niedrigsten Werte wurden insbesondere ",regions_fall2[si1$ix[length(rabbr)]]," registriert.")

# besser: die 10% der tiefsten und höchsten Werte aus der ganzen Schweiz bestimmen, dann Regionen suchen, wo solche Werte vorkommen und diese Regionen nennen

# Regionenmittel lassen sich auch bestimmen

# Unterschiede Hoch- und Tieflagen


alltext <- paste(text01.01,text01.02,text01.03,text01.04,sep=" ")
print(alltext)

sink("outfile2.tex")
cat(alltext)
sink()

