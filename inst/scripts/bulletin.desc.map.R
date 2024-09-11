bulletin.desc.map <- function(param,vdata,period) {

vtext <- list()

load('prec.masks.CH.regions.rda')
regions <- prec.masks.CH.regions

regions_fall1 <- c("Der Jura","Das westliche Mittelland","Das östliche Mittelland","Der westliche Alpennordhang",
             "Der östliche Alpennordhang","Das Wallis","Die Region Graubünden und Engadin","Die Alpensüdseite")
regions_fall2 <- c("im Jura","im westlichen Mittelland","im östlichen Mittelland","am westlichen Alpennordhang",
             "am östlichen Alpennordhang","im Wallis","im Kanton Graubünden und Engadin","auf der Alpensüdseite")

# Map analysis
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
	vtext$T$text01 <- paste0("Die Monatsmitteltemperaturen waren im ",period," schweizweit über dem Durchschnitt der Norm 1991-2020.")
	vtext$P$text01 <- paste0("Die Monatsniederschläge waren im ",period," schweizweit über dem Durchschnitt der Norm 1991-2020.")
} else if (max(allv) < -0.5) {
	vtext$T$text01 <- paste0("Die Monatsmitteltemperaturen waren im ",period," schweizweit unter dem Durchschnitt der Norm 1991-2020.")
	vtext$P$text01 <- paste0("Die Monatsniederschläge waren im ",period," schweizweit unter dem Durchschnitt der Norm 1991-2020.")
}

vtext$T$text02 <- paste0("Die Anomalien der Temperatur im ",period," lagen verbreitet zwischen ",qa[2],"°C und ",qa[3],"°C zur Norm.")
vtext$P$text02 <- paste0("Die Anomalien des Niederschlags im ",period," lagen verbreitet zwischen ",qa[2],"°C und ",qa[3],"\\% der Norm.")

vtext$T$text03 <- paste0("Lokal wurden auch niedrigere Abweichungen von bis zu ",qa[1],"°C und höhere Werte bis ",qa[4],"°C registriert.")
vtext$P$text03 <- paste0("Lokal wurden auch niedrigere Abweichungen von bis zu ",qa[1],"\\% und höhere Werte bis ",qa[4],"\\% registriert.")

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
vtext$T$text04 <- paste0(regions_fall1[si1$ix[1]]," wies insgesamt höhere Werte aus als andere Regionen. Die niedrigsten Werte wurden insbesondere ",regions_fall2[si1$ix[length(rabbr)]]," registriert.")
vtext$P$text04 <- paste0(regions_fall1[si1$ix[1]]," wies insgesamt höhere Werte aus als andere Regionen. Die niedrigsten Werte wurden insbesondere ",regions_fall2[si1$ix[length(rabbr)]]," registriert.")

alltext <- paste(vtext[[param]]$text01,vtext[[param]]$text02,vtext[[param]]$text03,vtext[[param]]$text04,sep=" ")
print(alltext)

sink(paste0(param,".desc.map.tex"))
cat(alltext)
sink()

}
