library(mchdwh)
library(colkd)

d <- dwhget_surface(param_short="tre200dx",year=2024,month=6,nat_abbr="BUS")
d2 <- dwhget_surface(param_short="tre200dn",year=2024,month=6,nat_abbr="BUS")

v <- d$value
t <- paste0(as.numeric(substr(d$datetime,7,8)),".",as.numeric(substr(d$datetime,5,6)),".")
v2 <- d2$value

cols <- colkd.temp.abs()

ar <- (max(v)-min(v2))/5

X11()
plot(0,xlim=c(1,length(v)),ylim=c(min(v2)-ar,max(v)+ar),type='n',xaxt='n',xlab="",ylab="Tageshöchst- und -Tiefsttemperatur (°C)",bty='n')
axis(side=1,at=1:length(v),labels=t,las=2)
rect(0.6,min(v2)-ar,length(v)+0.4,20,col=cols[12],border=cols[12])
rect(0.6,20,length(v)+0.4,25,col=cols[14],border=cols[14])
rect(0.6,25,length(v)+0.4,30,col=cols[16],border=cols[16])
rect(0.6,30,length(v)+0.4,max(v)+ar,col=cols[18],border=cols[18])
for (i in 1:length(v)) {
        rect(i-0.43,v[i],i+0.43,max(v)+ar+0.07,col="white",border="white")
}      
for (i in 1:length(v)) {
        rect(i-0.43,min(v2)-ar-0.07,i+0.43,v2[i],col="white",border="white")
}
for (i in 1:length(v)) {
        rect(i-0.6,min(v2)-ar-0.07,i-0.4,max(v)+ar+0.07,col="white",border="white")
}
abline(h=c(20,25,30),lty=2,col="grey70")
