library(ncdf4)
library(gridmch)
library(geocors)
library(mchdwh)
library(colkd)

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
file.name.prefix <- "niederschlag_monat_"
breaks <- c(15,35,50,65,80,95,105,120,140,180,220,300)

# Entscheid ob Vormonatswerte zu nehmen sind oder laufender Monat (wenn Datum > 21)
if (as.numeric(substr(enddate,9,10)) >= 21) {
# Laufender Monat: Mittel rechnen fuer bisherige Tage
product <- "RprelimD"
t.beg <- begdate
t.end <- enddate
t.format <- "yyyy.mm.dd"

# Berechnungs-Teil (hier nicht aendern)
pre <- gridmch(pname=pname,
            t.beg=t.beg,t.end=t.end,t.format=t.format,
            grid.name="ch01r.swiss.lv95",do.plot=FALSE,return.what="grid")
nrm.m <- gridmch(pname="RnormM9120",
            t.beg="2001.01",t.end="2001.12",t.format="yyyy.mm",
            grid.name="ch01r.swiss.lv95",do.plot=FALSE,return.what="grid")
nrm <- pre
days.in.month <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
for (dd in dimnames(nrm)[[3]]) {
   ii <- which(substr(dimnames(nrm.m)[[3]],5,6) == substr(dd,5,6))[1]
   nn <- days.in.month[ii]
   nrm[,,dd] <- nrm.m[,,ii]/nn
}
agg.pre <- apply(pre,FUN=sum,MARGIN=c(1,2))
agg.nrm <- apply(nrm,FUN=sum,MARGIN=c(1,2))
anom <- copy.grid.atts(pre,100*agg.pre/agg.nrm)
attr(anom,"time") <- attr(pre,"time")[1]
attr(anom,"grid.name") <- attr(pre,"grid.name")

# Graphik-Teil (hier nicht aendern)
plot.spec <- plot.specs(appear="Intranet", pname="RanomM9120",
                        user.opts=list(plot.stats=FALSE,breaks=breaks,
                                       main.pre=plot.title,main.mid="",
                                       fname.plot=file.name.prefix,
                                       method.label="",date.label="date"))
do.call(what = "gridmch.plot",
        args = c(list(grid=anom,dat=NULL,pname="RanomM9120",gr.format="pdf"),plot.spec))

vdata <- anom
} else {
#Letzter Monat laden und plot zeichnen
ym1 <- format(Sys.Date() %m-% months(1),"%Y.%m")

year <- as.numeric(substr(ym1,1,4))
mon  <- as.numeric(substr(ym1,6,7))
yearmon <- paste0(substr(ym1,1,4),substr(ym1,6,7))

m.beg <- paste0(substr(yearmon,1,4),".",substr(yearmon,5,6))
m.end <- m.beg
t.format <- "yyyy.mm"

# Berechnungs-Teil (hier nicht aendern)
ano <- gridmch(pname="RanomM9120",
            t.beg=m.beg,t.end=m.end,t.format=t.format,
            grid.name="ch01r.swiss.lv95",do.plot=FALSE,return.what="grid")

# Graphik-Teil (hier nicht aendern)
plot.spec <- plot.specs(appear="Intranet", pname="RanomM9120",
                        user.opts=list(plot.stats=FALSE,breaks=breaks,
                                       main.pre=plot.title,main.mid="",
                                       fname.plot=file.name.prefix,
                                       method.label="",date.label="date"))
do.call(what = "gridmch.plot",
        args = c(list(grid=ano,dat=NULL,pname="RanomM9120",gr.format="pdf"),plot.spec))

vdata <- ano

# Für einzelne Beispielstationen pro Region Werte aus DWH laden
stats <- c("BER","SMA","GVE","CHD","BAS","ENG","DAV","SIO","LUG","OTL","SAM","STG","GRC")

statval <- 0
ind_am <- 0
sname <- ""
c <- 0
for (i in stats) {
  c <- c+1
  tt <- dwhget_surface(nat_abbr = i,
                 param_short = c("rh9120mv"),
                 year=c(1864:year),meas_cat=1)
  statval[c] <- tt$value[which(substr(tt$datetime,1,6)==yearmon)]
  allm <- tt$value[which(substr(tt$datetime,5,6)==substr(yearmon,5,6))]
  ya <- as.numeric(substr(tt$datetime[which(substr(tt$datetime,5,6)==substr(yearmon,5,6))],1,4))
  names(allm) <- ya
  allm <- sort(allm,decreasing=T)
  ind_am[c] <- min(which(allm==statval[c]))
  sname[c] <- station_info(nat_abbr=i)$station_name
}
names(statval) <- sname

rg <- range(statval)
rgn <- 0
if (prod(rg)>0) {
        rgn[1] <- 0
        rgn[2] <- abs(max(rg))
} else {
        rgn <- rg
}
df <- abs(diff(rgn))/100

cols <-colkd.prec.anom()
statcol <- cut(sort(statval),breaks=c(-10000,breaks,10000),labels=F)

pdf("prec_dev_stats.pdf",width=10,height=8)
par(mar=c(5,17,1,1))
bp <- barplot(sort(statval),horiz=T,las=1,xlab="Prozent der Norm 1991-2020",
              col=cols[statcol],cex.axis=1.5,cex.names=1.5,cex.lab=1.5)
text(x=sort(statval)-df,y=bp,paste0(sort(statval)," (",ind_am[sort.int(statval,index.return=T)$ix],")"),adj=1,cex=1.5)
dev.off()
}

month <- c("Januar","Februar","März","April","Mai","Juni",
	   "Juli","August","September","Oktober","November","Dezember")

regions_fall1 <- c("Der Jura","Das westliche Mittelland","Das östliche Mittelland","Der westliche Alpennordhang",
             "Der östliche Alpennordhang","Das Wallis","Die Region Graubünden und Engadin","Die Alpensüdseite")
regions_fall2 <- c("im Jura","im westlichen Mittelland","im östlichen Mittelland","am westlichen Alpennordhang",
             "am östlichen Alpennordhang","im Wallis","im Kanton Graubünden und Engadin","auf der Alpensüdseite")

#######################################
# Was it too wet or too dry in general?
# What are the largest and the lowest values in the map?
allv <- vdata[!is.na(vdata)]

qa <- round(quantile(allv,prob=c(0,0.2,0.8,1)))

text01.01 <- ""
if (min(allv) > 110) {
	text01.01 <- paste0("Die monatlichen Niederschlagsmengen lagen im ",month[mon]," schweizweit über dem Durchschnitt der Norm 1991-2020.")
} else if (max(allv) < 90) {
	text01.01 <- paste0("Die monatlichen Niederschlagsmengen lagen im ",month[mon]," schweizweit unter dem Durchschnitt der Norm 1991-2020.")
}
text01.02 <- paste0("Die ",month[mon],"niederschläge betrugen verbreitet zwischen ",qa[2]," und ",qa[3]," Prozent der Norm.")
text01.03 <- paste0("Lokal wurden auch geringere Niederschlagsmengen von bis zu ",qa[1]," Prozent und höhere Werte bis ",qa[4]," Prozent der Norm 1991-2020 erreicht.")


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
text01.04 <- paste0(regions_fall1[si1$ix[1]]," wies insgesamt die grössten Niederschlagsmengen aus. Die niedrigsten Werte wurden insbesondere ",regions_fall2[si1$ix[length(rabbr)]]," registriert.")

# besser: die 10% der tiefsten und höchsten Werte aus der ganzen Schweiz bestimmen, dann Regionen suchen, wo solche Werte vorkommen und diese Regionen nennen

# Regionenmittel lassen sich auch bestimmen

# Unterschiede Hoch- und Tieflagen


alltext <- paste(text01.01,text01.02,text01.03,text01.04,sep=" ")
print(alltext)

sink("outfile3.tex")
cat(alltext)
sink()

