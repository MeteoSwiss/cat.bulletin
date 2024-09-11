bulletin.table <- function(stations,begdate,enddate,refabbr) {
library(clim.table)
library(mchdwh)

data <- clim.table::climtable(period=c(begdate,enddate))
vals <- data$dana$vals
vals$Region <- rep("",length(vals$Station))
vals$Region[1:14] <- "Westschweiz"
vals$Region[15:32] <- "Mittelland"
vals$Region[33:55] <- "Alpennordhang"
vals$Region[56:61] <- "Nord- und Mittelbünden"
vals$Region[62:70] <- "Wallis"
vals$Region[71:76] <- "Engadin"
vals$Region[77:88] <- "Alpensüdseite"

# generate subset for a printable table 
# (reduced to the stations defined in set.stations)
subset <- vals[which(vals$Station %in% stations),]
subset <- subset[order(match(subset$Station, stations)), ]
subset <- subset[,-c(6,10,14,15)]

sn <- station_info(nat_abbr=stations)
sn <- sn[order(match(sn$nat_abbr, stations)), ]
subset$Station <- sn$station_name
write.csv(subset, file = "climtable.subset.csv", row.names = FALSE, quote = FALSE)

