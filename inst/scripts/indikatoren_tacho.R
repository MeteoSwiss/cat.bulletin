library(mchdwh)

d <- dwhget_surface(param_short="ths200dx",nat_abbr="BER",year=c(1864,2024),month=6)

y <- as.numeric(substr(d$datetime,1,4))
ys <- unique(y)

sd <- 0
hd <- 0
for (i in 1:length(ys)) {
	ind <- which(y==ys[i])
	val <- d$value[ind]
	sd[i] <- length(which(val >= 25))
	hd[i] <- length(which(val >= 30))
}

sd_n <- sd[ys %in% 1991:2020]
sd_nm <- mean(sd_n)

hd_n <- hd[ys %in% 1991:2020]
hd_nm <- mean(hd_n)

# Tacho-Grafik
# Festlegen der Parameter
min_val <- 0
max_val <- 30
current_val <- sd[length(sd)]

# Berechnen der Winkel
theta_min <- pi
theta_max <- 0
theta_current <- pi * (1 - (current_val - min_val) / (max_val - min_val))

# Erstellen der Grafik
plot(NA, xlim=c(-1.5, 1.5), ylim=c(-1.1, 1.1), type="n", axes=FALSE, xlab="", ylab="",main="")

# Max-Segment einzeichnen
theta_mv <- seq(theta_min,pi * (1 - (max(sd) - min_val) / (max_val - min_val)),length=100)
x <- c(cos(theta_mv),rev(0.7 * cos(theta_mv)))
y <- c(sin(theta_mv),rev(0.7 * sin(theta_mv)))
polygon(x,y,col="grey70",border="grey70",lwd=2)

# Normsegment einzeichnen
theta_norm <- seq(theta_min,pi * (1 - (sd_nm - min_val) / (max_val - min_val)),length=100)
x <- c(cos(theta_norm),rev(0.7 * cos(theta_norm)))
y <- c(sin(theta_norm),rev(0.7 * sin(theta_norm)))
polygon(x,y,col="#FF9E8F",border="#FF9E8F",lwd=2)

# Zeichnen der halbrunden Achse
theta <- seq(theta_min, theta_max, length=100)
lines(cos(theta), sin(theta), col="black", lwd=2)

# Zeichnen der Skalenmarkierungen
for (i in seq(min_val, max_val, by=5)) {
  theta_i <- pi * (1 - (i - min_val) / (max_val - min_val))
  x <- cos(theta_i)
  y <- sin(theta_i)
  text(1.1 * x, 1.1 * y, labels=i, cex=1.2)
  segments(0.75 * x, 0.75 * y, x, y, col="black",lwd=2)
}
for (i in seq(min_val, max_val, by=1)) {
  theta_i <- pi * (1 - (i - min_val) / (max_val - min_val))
  x <- cos(theta_i)
  y <- sin(theta_i)
  segments(0.9 * x, 0.9 * y, x, y, col="black")
}

# Zeichnen des Zeigers
arrows(0, 0, 0.85 * cos(theta_current), 0.85 * sin(theta_current), col="red", lwd=3)

# Zeichnen des Zentrums
points(0, 0, pch=16, col="black", cex=1.5)
text(0,-0.15,"Sommertage",adj=0.5,cex=1.5)
text(0,-0.26,paste0("Juni 2024: ",sd[length(sd)]),adj=0.5,cex=1,col="red")
text(0,-0.37,paste0("Norm 1991-2020: ",round(sd_nm,1)),adj=0.5,cex=1,col="#FF9E8F")
text(0,-0.48,paste0("Maximum: ",max(sd)," (",ys[which(sd==max(sd))][1],")"),adj=0.5,cex=1,col="grey70")


# Vertikaler Balkenplot
X11(width=3,height=6)
par(mar=c(1,1,1,1))
plot(NA, xlim=c(-1.5, 2.5), ylim=c(-10, 32), type="n", axes=FALSE, xlab="", ylab="",main="")
rect(0,0,1,max(sd),col="grey70",border="grey70")
rect(0,0,1,sd_nm,col="#FF9E8F",border="#FF9E8F")
segments(0,0,0,max_val,lwd=2)
for (i in seq(min_val, max_val, by=1)) {
	lines(c(0,0.5),rep(i,2))
}
for (i in seq(min_val, max_val, by=5)) {
        lines(c(0,0.8),rep(i,2),lwd=2)
	text(-0.2,i,labels=i,adj=1)
}
lines(c(-0.1,1.1),rep(sd[length(sd)],2),col="red",lwd=4)
text(-0.375,-2.5,"Sommertage",adj=0,cex=1.2)
text(-0.375,-4,paste0("Juni 2024: ",sd[length(sd)]),adj=0,cex=0.8,col="red")
text(-0.375,-5.3,paste0("Norm 1991-2020: ",round(sd_nm,1)),adj=0,cex=0.8,col="#FF9E8F")
text(-0.375,-6.6,paste0("Maximum: ",max(sd)," (",ys[which(sd==max(sd))][1],")"),adj=0,cex=0.8,col="grey70")

