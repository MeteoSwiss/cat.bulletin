library(gridmch)
source('bulletin.maps.R')
source('period.to.analyse.R')
load('prec.masks.CH.regions.rda')

prec.masks.CH.regions2 <- prec.masks.CH.regions
prec.masks.CH.regions2$spw[!is.na(prec.masks.CH.regions$spe)] <- 1
prec.masks.CH.regions2$spw[!is.na(prec.masks.CH.regions$jum)] <- 1
prec.masks.CH.regions2$spw[!is.na(prec.masks.CH.regions$nse)] <- 1
prec.masks.CH.regions2$spw[!is.na(prec.masks.CH.regions$nsw)] <- 1
prec.masks.CH.regions2$spw[!is.na(prec.masks.CH.regions$val)] <- 1
prec.masks.CH.regions2$spw[!is.na(prec.masks.CH.regions$gre)] <- 1
prec.masks.CH.regions2$spe <- NULL
prec.masks.CH.regions2$nse <- NULL
prec.masks.CH.regions2$jum <- NULL
prec.masks.CH.regions2$val <- NULL
prec.masks.CH.regions2$gre <- NULL
prec.masks.CH.regions2$nsw <- NULL


# Load the ncdf4 library
library(ncdf4)

# Specify the path to your NetCDF file
nc_file <- "/repos/repos_data/climate/grid/mch.grids/Misc/topo.swiss02_ch01r.swisscors.nc"

# Open the NetCDF file
nc_data <- nc_open(nc_file)

# Specify the name of the 2D variable you want to read (replace 'variable_name' with the actual variable name)
variable_name <- "height"

# Get the 2D variable data
topo <- ncvar_get(nc_data, variable_name)

# Close the NetCDF file
nc_close(nc_data)

dates <- anaperiod(timespan = "m",year=1992,period=1,ref_period = c(1991,2020))

map <- bulletin.maps(param="T",type="anom",
                     begdate=dates$begdate,enddate=dates$enddate,
                     status=dates$status,timespan=dates$timespan,
                     refabbr=dates$refabbr)

qm1 <- quantile(map,probs = c(0.15,0.85),na.rm=T)

regs2 <- c("spw","ssa")

mapcut <- cut(map,breaks=c(-Inf,-1,1,Inf),labels = F)
mapcut <- matrix(mapcut,370,240)

reglow <- array(NA,c(length(regs2),3))
reghigh <- array(NA,c(length(regs2),3))
regqslow <- array(NA,c(length(regs2),2))
regqshigh <- array(NA,c(length(regs2),2))
for (i in 1:length(regs2)) {
  indlow <- which(!is.na(prec.masks.CH.regions2[[regs2[i]]]) & topo<1000)
  indhigh <- which(!is.na(prec.masks.CH.regions2[[regs2[i]]]) & topo>=1000)
  vlow <- mapcut[indlow]
  vfactor <- factor(vlow, levels = c(1, 2, 3))
  vcount <- table(vfactor)
  perclow <- round(vcount/length(indlow),digits=2)*100
  vhigh <- mapcut[indhigh]
  vfactor <- factor(vhigh, levels = c(1, 2, 3))
  vcount <- table(vfactor)
  perchigh <- round(vcount/length(indhigh),digits=2)*100
  reglow[i,] <- perclow
  reghigh[i,] <- perchigh
  regqslow[i,] <- round(quantile(map[indlow],probs = c(0.15,0.85)),digits = 1)
  regqshigh[i,] <- round(quantile(map[indhigh],probs = c(0.15,0.85)),digits = 1)
}

#Check first whether all gridpoints fall in 1 category
case_check <- all(mapcut %in% 1:3)

thresh_verb <- 55
thresh_reg <- 25

vergnorm <- c("unter der Norm","im Bereich der Norm","über der Norm")
norm <- "1991-2020"

text01 <- ""
text02 <- ""
text03 <- ""
text04 <- ""
if (case_check) {
  vn1 <- unique(as.vector(mapcut[!is.na(mapcut)]))
  text01 <- paste0("Die Monatsmitteltemperatur lag schweizweit ",vergnorm[vn1]," ",norm,". Die Abweichungen erreichten in weiten Teilen des landes zwischen ",qm1[1]," und ",qm1[2],"°C.")
} else {
  #Alpennordseite, tiefe Lagen
  if (any(reglow[1,]>thresh_verb)) {
    vn1 <- which(reglow[1,]>thresh_verb)
    text01 <- paste0("Auf der Alpennordseite wurden in tiefen Lagen verbreitet Werte ",vergnorm[vn1]," ",norm," verzeichnet.")
    text01.01 <- ""
    if (any(reglow[1,]<=thresh_verb & reglow[1,]>thresh_reg)) {
      vn2 <- which(reglow[1,]<=thresh_verb & reglow[1,]>thresh_reg)
      text01.01 <- paste0(" Regional wurden auch Temperaturen ",vergnorm[vn2]," erreicht.")
    }
    text01.02 <- paste0(" Insgesamt lagen die Temperaturabweichungen nördlich des Alpenhauptkamms in tieferliegenden Regionen etwa zwischen ",regqslow[1,1]," und ",regqslow[1,2],"°C.")
    text01 <- paste0(text01,text01.01,text01.02)
  }
  #Alpennordseite, hohe Lagen
  if (any(reghigh[1,]>thresh_verb)) {
    vn1 <- which(reghigh[1,]>thresh_verb)
    text02 <- paste0("In hohen Lagen wurden nördlich der Alpen mehrheitlich Monatsmitteltemperaturen ",vergnorm[vn1]," ",norm," registriert.")
    text02.01 <- ""
    if (any(reghigh[1,]<=thresh_verb & reghigh[1,]>thresh_reg)) {
      vn2 <- which(reghigh[1,]<=thresh_verb & reghigh[1,]>thresh_reg)
      text02.01 <- paste0(" Gebietsweise lagen die Temperaturen in höheren Lagen auch ",vergnorm[vn2],".")
    }
    text02.02 <- paste0(" Es wurden Abweichungen zwischen ",regqshigh[1,1]," und ",regqshigh[1,2],"°C verzeichnet.")
    text02 <- paste0(text02,text02.01,text02.02)
  }
  #Südseite, tiefe Lagen
  if (any(reglow[2,]>thresh_verb)) {
    vn1 <- which(reglow[2,]>thresh_verb)
    text03 <- paste0("In der Südschweiz zeigten sich in den Tieflagen grossteils monatliche Durchschnittswerte ",vergnorm[vn1]," ",norm,".")
    text03.01 <- ""
    if (any(reglow[2,]<=thresh_verb & reglow[2,]>thresh_reg)) {
      vn2 <- which(reglow[2,]<=thresh_verb & reglow[2,]>thresh_reg)
      text03.01 <- paste0(" Regional wurden auch Temperaturen ",vergnorm[vn2]," erreicht.")
    }
    text03.02 <- paste0(" Insgesamt lagen die Temperaturdifferenzen zur Norm südlich des Alpenhauptkamms in Tieflagen bei ca. ",regqslow[2,1]," bis ",regqslow[2,2],"°C.")
    text03 <- paste0(text03,text03.01,text03.02)
  }
  #Südseite, höhere Lagen
  if (any(reghigh[2,]>thresh_verb)) {
    vn1 <- which(reghigh[2,]>thresh_verb)
    text04 <- paste0("Die höhergelegenen Gebiete auf der Alpensüdseite brachten primär Monatstemperaturen ",vergnorm[vn1]," ",norm,".")
  }
  text04.01 <- ""
  if (any(reghigh[2,]<=thresh_verb & reghigh[2,]>thresh_reg)) {
    vn2 <- which(reghigh[2,]<=thresh_verb & reghigh[2,]>thresh_reg)
    text04.01 <- paste0(" Stellenweise lagen die Temperaturen in erhöhten Lagen auch ",vergnorm[vn2],".")
  }
  text04.02 <- paste0(" Es wurden Abweichungen von der Norm zwischen ",regqshigh[2,1]," und ",regqshigh[2,2],"°C gemessen.")
  text04 <- paste0(text04,text04.01,text04.02)
}
alltext <- paste(text01,text02,text03,text04)
