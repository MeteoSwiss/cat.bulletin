library(clim.table)
library(mchdwh)

library(lubridate)

data <- clim.table::climtable(period=c("20230101","20230131"))
vals <- data$dana$vals
vals$Region <- rep("",length(vals$Station))
vals$Region[1:14] <- "Westschweiz"
vals$Region[15:32] <- "Mittelland"
vals$Region[33:55] <- "Alpennordhang"
vals$Region[56:61] <- "Nord- und Mittelbünden"
vals$Region[62:70] <- "Wallis"
vals$Region[71:76] <- "Engadin"
vals$Region[77:88] <- "Alpensüdseite"
vals$Subregion <- rep("",length(vals$Station))
vals$Subregion[c(4,10,11,14,22)] <- "Westliches Mittelland"
vals$Subregion[c(2,3,5,6,7,15,16)] <- "Jura"
vals$Subregion[c(12,33,34)] <- "Westalpen"
vals$Subregion[c(1,8,9,13)] <- "Genferseeregion"
vals$Subregion[c(19,20,21,25)] <- "Nordschweiz"
vals$Subregion[c(17,18,23,24)] <- "Nordwestschweiz"
vals$Subregion[c(26,27,28,29,30,31,32)] <- "Nordostschweiz"
vals$Subregion[c()] <- "Östliche Voralpen und Alpen"
vals$Subregion[c()] <- "Rhônetal"
vals$Subregion[c()] <- "Rheintal"
vals$Subregion[56:61] <- "Zentralalpen"
vals$Subregion[56:61] <- "Nord- und Mittelbünden"
vals$Region[71:76] <- "Engadin"
vals$Region[77:88] <- "Alpensüdseite"





stats <- data$dana$vals$Station

years <- c(1991,2023)

data <- dwhget_surface(param_short="tre200mv",year=years,nat_abbr=stats)

stats <- unique(data$nat_abbr)
dates <- unique(data$datetime)

statrem <- ""
for (i in 1:length(stats)) {
  ind <- which(data$nat_abbr %in% stats[i])
  if (length(ind)<length(dates)) {
    statrem <- c(statrem,stats[i])
  }
}
statrem <- statrem[-1]
stats <- stats[-which(stats %in% statrem)]

datanew <- array(NA,c(length(stats),length(dates)))
for (i in 1:length(stats)) {
  ind <- which(data$nat_abbr %in% stats[i])
  datanew[i,] <- data$value[ind]
}
# scaling
means <- apply(datanew,2,mean)
sds <- apply(datanew,2,sd)
datanew2 <- scale(datanew,center = means, scale = sds)

datanew2 <- as.data.frame(datanew2)
attributes(datanew2)$names <- dates
attributes(datanew2)$row.names <- stats
attributes(datanew)$names <- dates
attributes(datanew)$row.names <- stats

datanew2 <- t(datanew2)
datanew <- t(datanew)

distance <- dist(datanew2)

mydata.hclust = hclust(distance)
plot(mydata.hclust,cex=0.4)

member = cutree(mydata.hclust,10)
table(member)

# Mittel über alle Member in einem Cluster (hier: 1)
apply(datanew[as.numeric(which(member==1)),],2,mean)

# Einfach Abweichungen zur Norm für die 88 Stationen aus dem DWH holen statt climtable rechnen --> climtable nur für aktuellen Monat