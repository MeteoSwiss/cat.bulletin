absdata <- read.table("climate-temperature-evolution-region-abs_ths200m0_abs_loess30_02_region_regSwiss_de.txt",header=T)
testdata <- read.table("climate-temperature-evolution-region-anom_ths200m0_anom9120_loess30_02_region_regSwiss_de.txt",header=T)

prec <- read.table("climate-precipitation-evolution-region-anom_rhs150m0_anom9120_loess30_02_region_regSwiss_de.txt",header=T)
p <- prec$val

percentile <- ecdf(p)
pp <- abs(percentile(1.4)-0.5)
# anstelle von 1.4 jeden Wert von p und v rechnen, dann den aussergewöhnlichen Wert als erstes wählen

a <- absdata$val

ystart <- 1980
y <- testdata$year
v <- testdata$val

ycorr <- testdata$val-absdata$val
ycorr <- ycorr[1]

ylast <- y[length(y)]
yshow <- ystart:ylast
indy <- which(y %in% yshow)
vlast <- v[indy]

v9120 <- v[which(y %in% 1991:2020)]

vsd <- sort.int(v,decreasing=T,index.return=T)
vsi <- sort.int(v,decreasing=F,index.return=T)

cols <- rep("blue",length(indy))
cols[vlast >= 0] <- "red"

#################

X11(width=11,height=6)
nf <- layout( matrix(c(1,2), ncol=2), widths=c(6,1))
par(mar=c(5.1, 4.1, 4.1, 0.5))
plot(0,type='n',xlim=c(ystart,ylast),ylim=c(min(v),max(v)),ylab="Monatliche Temperaturabweichung [°C]",xlab="",xaxt="n",yaxt="n",main="Abweichung der landesweiten Monatstemperatur zur Norm 1991-2020")
a1 <- axis(side=1,at=seq(ystart,ylast,10))
abline(v=a1,lty=2,col="grey70")
a11 <- axis(side=1,at=seq(ystart,ylast,1),labels=FALSE,tck=-0.01)
a2 <- axis(side=2,at=seq(-10,4,2),las=2)
a2[a2==0]
abline(h=a2[a2!=0],lty=2,col="grey70")
for (i in 1:length(indy)) {
        rect(yshow[i]-0.35,0,yshow[i]+0.35,vlast[i],col=cols[i],border=cols[i])
}
abline(h=0)

par(mar=c(5.1, 0.5, 4.1, 0.5))
plot(0,type='n',xlab="",xaxt="n",yaxt="n",main="",ylab="",ylim=c(min(v),max(v)),xlim=c(0,1),bty='n')
mtext(side=1,text="Messbeginn\n1864",line=1)
rect(0,min(v),0.1,max(v),col="grey70",border="grey70")
#rect(0,min(v9120),0.1,max(v9120),col="grey50",border="grey50")
lines(c(0,0.1),c(0,0))
poshigh    <- v[vsd$ix[1]]
poshigh[2] <- v[vsd$ix[1]]-max(v[vsd$ix[1]]-v[vsd$ix[2]],(max(v)-min(v))/23.7)
poshigh[3] <- poshigh[2]-max(v[vsd$ix[2]]-v[vsd$ix[3]],(max(v)-min(v))/23.7)
for (i in 1:3) {
  lines(c(0,0.1),c(v[vsd$ix[i]],v[vsd$ix[i]]),col="red")
  text(0.34,poshigh[i],labels=y[vsd$ix[i]],adj=0,col="red")
  arrows(0.31,poshigh[i],0.13,v[vsd$ix[i]],col="red",length=0.06)
}
poslow    <- v[vsi$ix[1]]
poslow[2] <- v[vsi$ix[1]]+max(v[vsi$ix[2]]-v[vsi$ix[1]],(max(v)-min(v))/23.7)
poslow[3] <- poslow[2]+max(v[vsi$ix[3]]-v[vsi$ix[2]],(max(v)-min(v))/23.7)
for (i in 1:3) {
  lines(c(0,0.1),c(v[vsi$ix[i]],v[vsi$ix[i]]),col="blue")
  text(0.34,poslow[i],labels=y[vsi$ix[i]],adj=0,col="blue")
  arrows(0.31,poslow[i],0.13,v[vsi$ix[i]],col="blue",length=0.06)
}


################

X11(width=3,height=8)
plot(0,type='n',xlab="",xaxt="n",yaxt="n",main="",ylab="Temperaturabweichung zur Norm 1991-2020 [°C]",ylim=c(min(v),max(v)),xlim=c(0,1),bty='n')
axis(side=2,las=2)
rect(0,min(v),0.2,max(v),col="grey80",border="grey80")
rect(0,min(v9120),0.2,max(v9120),col="grey60",border="grey60")
cols <- rep("grey40",length(v))
cols[length(cols)] <- "red"
for (i in 1:length(v)) {
  lines(c(0,0.2),c(v[i],v[i]),col=cols[i],lend=1,lwd=1.5)
  if (i == length(v)) {
	  text(0.25,v[length(v)],labels=y[i],adj=0,col="red")
  }
}
lines(c(0,0.2),c(0,0),lend=1,lwd=3)


##################

topbound <- max(v)+(max(v)-min(v))/4
topbound_white <- max(v)+(max(v)-min(v))/5
lowbound <- min(v)-(max(v)-min(v))/2.5
circle_center <- min(v)-(max(v)-min(v))/4.87

library(plotrix)
X11(width=3,height=8)
par(mar=c(5.1,4.1,4.1,4))
plot(0,type='n',xlab="",xaxt="n",yaxt="n",main="",ylab="",ylim=c(lowbound,topbound),xlim=c(0,1),bty='n')
at1 <- pretty(v,round(max(v)-min(v),digits=0))
at1l <- at1
at1l[at1l>0] <- paste0("+",at1l[at1l>0])
axis(side=4,las=2,at=at1,line=-2,labels=at1l)
at2 <- pretty(a,round(max(a)-min(a),digits=0))
axis(side=2,las=2,at=at2+ycorr,labels=at2,line=-2)
#draw.circle(0.5,circle_center,0.4,col="black",border="black")
#rect(0.2,circle_center,0.8,topbound,col="black",border="black")
#draw.circle(0.5,circle_center,0.3,col="white",border="white")
#rect(0.3,circle_center,0.7,topbound_white,col="white",border="white")
draw.circle(0.5,circle_center,0.2,col="red",border="red")
rect(0.4,circle_center,0.6,v[length(v)],col="red",border="red")
abline(h=v[length(v)],col="red",lty=2,lwd=2)
text(1.08,v[length(v)],paste0("+",round(v[length(v)],digits=1)),col="red",xpd=NA,adj=0)
text(-0.08,a[length(a)]+ycorr,round(a[length(a)],digits=1),col="red",xpd=NA,adj=1)
mtext("°C",side=2,las=2)
mtext("°C",side=4,las=2)
text(0.75,7.3,"Abweichung\nzur Norm\n1991-2020",adj=0,xpd=NA)
text(0.25,7.3,"Monats-\nmittel\ntemperatur",adj=1,xpd=NA)

