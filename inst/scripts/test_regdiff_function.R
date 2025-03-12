#Input
mondate <- formatC(8,width=2, flag="0")
begdate <- paste0(2024,mondate,"01")
enddate <- paste0(2024,mondate,datefuns::days.of.mon(2024, 8))

data <- clim.table::climtable(period=c(begdate,enddate), outDir = ".")



# set region every station is belonging to
regsort <- c("Jura","Mittelland","Alpennordhang","Wallis","Nord- und Mittelbünden","Engadin","Alpensüdseite")
vals <- data$dana$vals

station_info <- mchdwh::station_info(nat_abbr = vals$Station, region_type_id = 1)
rownames(station_info) <- station_info$nat_abbr
vals$Region <- station_info[vals$Station, "region_name_G"]
vals$Region <- clean_region_names(vals$Region)
assert_that(all(vals$Region %in% regsort), msg = "The regsort vector does not correspond to region values of the stations")

# Computation of regional differences in temperature, precipitation and sunshine duration
comp_regdiff <- function(parameter, vals) {

  # Fixed input, parameter-independent
  dev_probs <- c(0.16, 0.84)
  alt_limit <- 1500
  standard_stations <- c("BER","SMA","GVE","BAS","ENG","DAV","SIO","LUG","SAM")
  
  if (parameter == "T") {
    # Fixed input for temperature
    deviations <- "Abw"
    norm_range <- c(-0.5,0.5)
    tab_columns <- c(1:5)
    climtab_names <- c("Station","Höhe (m)","Monatsmittel (\u00B0C)","Referenz (\u00B0C)","Abweichung (\u00B0C)")
  }
  if (parameter == "P") {
    # Fixed input for precipitation
    deviations <- "R.dev"
    norm_range <- c(95,105)
    tab_columns <- c(1,11:13)
    climtab_names <- c("Station","Monatssumme (mm)","Referenz (mm)","Verhältnis zur Referenz (%)")
  }
  if (parameter == "S") {
    # Fixed input for sunshine duration
    deviations <- "S.dev"
    norm_range <- c(95,105)
    tab_columns <- c(1,7:9)
    climtab_names <- c("Station","Monatssumme (h)","Referenz (h)","Verhältnis zur Referenz (%)")
  }
  
  acurr_all <- vals[[deviations]][!is.na(vals[[deviations]])]
  a_ueber <- length(which(acurr_all > norm_range[2])) / length(acurr_all)
  a_unter <- length(which(acurr_all < norm_range[1])) / length(acurr_all)
  a_bereich <- 1 - a_ueber - a_unter
  quac <- quantile(acurr_all,probs = dev_probs)
  if (parameter == "T") {
    quac[quac>0] <- paste0("+",quac[quac>0])
  }
  
  # temperature difference with altitude
  diff_highlow <- abs(median(vals[[deviations]][vals$Hoehe>=alt_limit],na.rm=T))-abs(median(vals[[deviations]][vals$Hoehe<alt_limit],na.rm=T))
  
  # greatest vals[[deviations]] in all regions
  regs <- unique(vals$Region)
  vhighest <- 1
  vlowest <- 1
  for (r in 1:length(regs)) {
    indr <- which(vals$Region == regs[r])
    vhighest[r] <- quantile(abs(vals[[deviations]][indr]),0.75,na.rm=TRUE)
    vlowest[r] <- quantile(abs(vals[[deviations]][indr]),0.25,na.rm=TRUE)
  }
  member_h <- stats::cutree(hclust(dist(vhighest)),3)
  member_l <- stats::cutree(hclust(dist(vlowest)),3)
  mr_h <- 0
  mr_l <- 0
  for (i in 1:3) {
    indm_h <- which(member_h == i)
    mr_h[i] <- mean(vhighest[indm_h])
    indm_l <- which(member_l == i)
    mr_l[i] <- mean(vlowest[indm_l])
  }
  mhigh <- which(mr_h==max(mr_h))
  mhigh <- which(member_h == mhigh)
  regshigh <- collapse_sentence(regs[mhigh])
  mlow <- which(mr_l==min(mr_l))
  mlow <- which(member_l == mlow)
  regslow <- collapse_sentence(regs[mlow])
  # add a few relevant stations to the list below
  selhigh <- vals[which(vals$Region %in% regs[mhigh]),]
  selhigh <- selhigh[order(match(selhigh[[deviations]], sort(selhigh[[deviations]],decreasing = TRUE))), ]
  selhigh <- selhigh[1:2,]
  sellow <- vals[which(vals$Region %in% regs[mlow]),]
  sellow <- sellow[order(match(sellow[[deviations]], sort(sellow[[deviations]],decreasing = FALSE))), ]
  sellow <- sellow[1:2,]
  sellow_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=sellow$Station)$station_name)
  selhigh_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=selhigh$Station)$station_name)
  selreg <- rbind(selhigh,sellow)
  selreg <- selreg[!(selreg$Station %in% standard_stations), ]
  
  # generate subset for a printable table
  # (reduced to the stations defined above)
  subset_climtab <- vals[which(vals$Station %in% standard_stations),]
  subset_climtab <- subset_climtab[order(match(subset_climtab$Station, standard_stations)), ]
  subset_climtab <- rbind(subset_climtab,selreg)
  subset_climtab$Region <- factor(subset_climtab$Region, levels = regsort, ordered = TRUE)
  subset_climtab <- subset_climtab[order(subset_climtab$Region),]
  subset_climtab <- subset_climtab[,tab_columns]
  sn <- mchdwh::station_info(nat_abbr=subset_climtab$Station)
  sn <- sn[order(match(sn$nat_abbr, subset_climtab$Station)), ]
  subset_climtab$Station <- sn$station_name
  if (parameter == "T") {
    subset_climtab[[deviations]][subset_climtab[[deviations]] > 0] <- paste0("+", subset_climtab[[deviations]][subset_climtab[[deviations]] > 0])
  }
  rownames(subset_climtab) <- NULL
  attributes(subset_climtab)$names <- climtab_names
  
  # Output
  return(list(allvalues = acurr_all, anteil_ueber = a_ueber, anteil_unter = a_unter, anteil_bereich = a_bereich,
  quantiles = quac, numb_stats_high = mhigh, regshigh = regshigh, numb_stats_low = mlow, regslow = regslow,
  selhigh_stats = selhigh_stats, sellow_stats = sellow_stats, selhigh_abw = selhigh[[deviations]], 
  sellow_abw = sellow[[deviations]], diff_highlow = diff_highlow, climtab_vals = vals, subset_climtab = subset_climtab))
  
}


clean_region_names <- function(regions) {
  regions <- sub(".*(Alpennordhang|Mittelland|Jura).*", "\\1", regions)
  return(regions)
}
