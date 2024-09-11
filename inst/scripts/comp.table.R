library(clim.table)
library(mchdwh)

data <- clim.table::climtable(period=c("202405","202405"))
vals <- data$dana$vals
vals$Region <- rep("",length(vals$Station))
vals$Region[1:14] <- "Westschweiz"
vals$Region[15:32] <- "Mittelland"
vals$Region[33:55] <- "Alpennordhang"
vals$Region[56:61] <- "Nord- und Mittelbünden"
vals$Region[62:70] <- "Wallis"
vals$Region[71:76] <- "Engadin"
vals$Region[77:88] <- "Alpensüdseite"

regs <- unique(vals$Region)

TTanom_mean <- 0
TTanom_min <- 0
TTanom_max <- 0
Stats_TTanom_max <- ""
Stats_TTanom_min <- ""
for (r in 1:length(regs)) {
	reg <- regs[r]
	ireg <- which(vals$Region == reg)
	print(paste0("Berechnungen für Region ",reg,":"))

	# Temperature deviations from the norm
	Stats <- vals$Station[ireg]
        TTanom <- vals$Abw[ireg]
	TTanom_mean[r] <- round(mean(TTanom,na.rm=T),digits=1)
	TTanom_min[r] <- min(TTanom,na.rm=T)
	TTanom_max[r] <- max(TTanom,na.rm=T)

	# Station(s) with max. temperature deviation
        Stats_TTanom_min[r] <- Stats[which(TTanom == TTanom_min[r])[1]]
	Stats_TTanom_max[r] <- Stats[which(TTanom == TTanom_max[r])[1]]
}

regdata <- cbind(TTanom_mean,TTanom_min,TTanom_max)
row.names(regdata) <- regs

text01 <- paste0("In der Westschweiz wurden Temperaturabweichungen zur Norm 1991-2020 von ",regdata[1,2]," bis ",regdata[1,3]," °C registriert. Im Mittelland lagen die Temperaturabweichungen im Bereich von ",regdata[2,2]," und ",regdata[2,3]," °C. Messstandorte am Alpennordhang lieferten Abweichungen von ",regdata[3,2]," bis ",regdata[3,3]," °C, in Nord- und Mittelbünden waren es ",regdata[4,2]," bis ",regdata[4,3]," °C. Im Wallis wich die Temperatur zwischen ",regdata[5,2]," und ",regdata[5,3]," °C von der Norm 1991-2020 ab. Temperaturanomalien von ",regdata[6,2]," bis ",regdata[6,3]," °C meldete das Engadin. Auf der Alpensüdseite lagen die Abweichungen im Bereich von ",regdata[7,2]," und ",regdata[7,3]," °C.")

if (length(which(vals$Abw==max(vals$Abw,na.rm=T))) == 1) {
	text02 <- paste0("Die höchste Abweichung zur Norm 1991-2020 von ",max(vals$Abw,na.rm=T)," °C wurde am Messstandort ",vals$Station[which(vals$Abw==max(vals$Abw,na.rm=T))]," erreicht.")
}
if (length(which(vals$Abw==max(vals$Abw,na.rm=T))) == 2) {
	text02 <- paste0("Die höchste Abweichung zur Norm 1991-2020 von ",max(vals$Abw,na.rm=T)," °C wurde an den Messstandorten ",paste0(vals$Station[which(vals$Abw==max(vals$Abw,na.rm=T))],collapse=" und ")," erreicht.")
}





#
## In welchen Regionen wurden die höchsten und tiefsten Abweichungen der Temperatur von der Norm registriert?
#dTTm <- (max(TTanom_mean)-min(TTanom_mean))/5
#minval_TT <- min(TTanom_mean)+dTTm
#maxval_TT <- max(TTanom_mean)-dTTm
#
#regTTmin <- which(TTanom_mean < minval_TT)
#regTTmax <- which(TTanom_mean > maxval_TT)
#
#if (length(regTTmax) == 1) {
#	paste0("Die grössten Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 wurden in der Region ",regs[regTTmax]," registriert.")
#}
#if (length(regTTmax) == 2) {
#        paste0("Die grössten Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 wurden in den Regionen ",regs[regTTmax[1]]," und ",regs[regTTmax[2]]," registriert.")
#}
#if (length(regTTmax) == 3) {
#        paste0("Die grössten Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 wurden in den Regionen ",regs[regTTmax[1]],", ",regs[regTTmax[2]]," und ",regs[regTTmax[3]]," registriert.")
#}
#
#if (length(regTTmin) == 1) {
#        paste0("Die tiefsten Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 wurden in der Region ",regs[regTTmin]," registriert.")
#}
#if (length(regTTmin) == 2) {
#        paste0("Die tiefsten Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 wurden in den Regionen ",regs[regTTmin[1]]," und ",regs[regTTmin[2]]," registriert.")
#}
#if (length(regTTmin) == 3) {
#        paste0("Die tiefsten Abweichungen der Monatsmitteltemperatur zur Norm 1991-2020 wurden in den Regionen ",regs[regTTmin[1]],", ",regs[regTTmin[2]]," und ",regs[regTTmin[3]]," registriert.")
#}
#
## In welchen Höhenstufen wurden die höchsten resp. tiefsten Werte registriert?
#hl <- cut(vals$Hoehe,breaks=c(0,400,700,1000,1500,2000,2500,5000),labels=F)
#TTanom_mean_hl <- 0
#for (i in 1:7) {
#	indhl <- which(hl==i)
#	TTanom_mean_hl[i] <- round(mean(vals$Abw[indhl],na.rm=T),1)
#}
