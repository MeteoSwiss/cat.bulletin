library(evoclim)
library(mchdwh)
library(colkd)

#swissmean anom
homogval.evol(homog.param = "ths200m0", stations="swissmean", begin.date = "186401", end.date = "202408",
                add.current.mo = FALSE, val.resolution = "m", anomalies = TRUE, begin.refperiod = "1991",
                end.refperiod = "2020", data.line = "loess", 
                loess.param = list (window=30,conf.int=TRUE,do.diff=TRUE,y1=1885,y2=NULL,y1.as.mean=TRUE,add.exp.val=TRUE),
                add.ranking.high = TRUE, add.ranking.low = FALSE, add.legend = TRUE, add.source = TRUE, out.path = "current",
                language = "d", sig.tool.time = TRUE, write.txt = TRUE, axis.cex = 1.2)

monname <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
month <- c("J","F","M","A","M","J","J","A","S","O","N","D")

amon <- NA
rank <- rep(NA,12)
for (m in 1:length(monname)) {
  fname <- paste0("ths200m0.swissmean.m.",monname[m],".1864.*.anom.txt")
  ff <- list.files(pattern = fname)
  data_anom <- read.table(ff, header=T)
  year <- data_anom$year
  ycurr <- year[length(year)]
  if (ycurr == 2024) {
    anom <- data_anom$val
    poscurr <- length(year)
    amon[m] <- round(anom[poscurr],1)
    rank[m] <- which(sort.int(anom,decreasing = TRUE,index.return = TRUE)$ix==poscurr)
  } else {
    amon[m] <- NA
  }
}
amon_cat <- cut(amon,breaks = c(-Inf,-7,-6,-5,-4,-3,-2,-1.5,-1,-0.5,0.5,1,1.5,2,3,4,5,6,7,Inf),labels = FALSE)
amon_col <- colkd.temp.anom()[amon_cat]
amon_text <- as.character(amon)
amon_text[amon>0 & !is.na(amon)] <- paste0("+",amon_text[amon>0 & !is.na(amon)])

X11(width=10,height=4)
plot(0,type='n',xlim=c(0,13),ylim=c(-0.9,0.2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",
     main="Monatliche Temperaturabweichungen zur Norm 1991-2020",cex.main=1.5)
if (any(is.na(amon_col))) {
  ina <- which(is.na(amon_col))
  lines(c(ina[1]-1,ina[length(ina)]),c(0,0),lwd=6,col="grey70")
  points(ina,rep(0,length(ina)),cex=3.3,pch=21,col="grey70",bg="grey70",lwd=3)
  text(ina,-0.4,month[ina],col="grey70",font = 2)
}
inna <- which(!is.na(amon_col))
lines(c(inna[1],inna[length(inna)]),c(0,0),lwd=6,col="black")
points(inna,rep(0,length(inna)),cex=3.3,pch=21,col="black",bg=amon_col[!is.na(amon_col)],lwd=3)
text(inna,-0.4,month[inna],col="black",font = 2)
text(1:12,-0.55,amon_text,col="black")
text(0.4,-0.55,"[°C]",adj=1, font = 2)
text(0.4,0,"2024",cex=1.5,adj=1, font = 2)
text(1:12,-0.7,rank,col="black")
text(0.4,-0.7,"Rang",adj=1, font = 2)



