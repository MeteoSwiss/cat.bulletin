calculate_regional_differences <- function(bulletin) {
  
  # prepare climtable
  stations <- c("BER","SMA","GVE","BAS","ENG","SIO","LUG","SAM")
  
  if (bulletin$month<10) {mondate <- paste0("0",bulletin$month)} else {mondate <- as.character(bulletin$month)}
  begdate <- paste0(bulletin$year,mondate,"01")
  dpm <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if (bulletin$year %% 4 == 0) {dpm <- c(31,29,31,30,31,30,31,31,30,31,30,31)}
  enddate <- paste0(bulletin$year,mondate,dpm[bulletin$month])
  
  fname_climtable <- paste0(bulletin$data_path,"/climtable.RData")
  if (file.exists(fname_climtable)) {
    load(fname_climtable)
  } else {
    data <- clim.table::climtable(period=c(begdate,enddate))
    save(data, file = fname_climtable)
  }
  
  # set region every station is belonging to
  vals <- data$dana$vals
  vals$Region <- rep("",length(vals$Station))
  vals$Region[1:14] <- "Westschweiz"
  vals$Region[15:32] <- "Mittelland"
  vals$Region[33:55] <- "Alpennordhang"
  vals$Region[56:61] <- "Nord- und Mittelbünden"
  vals$Region[62:70] <- "Wallis"
  vals$Region[71:76] <- "Engadin"
  vals$Region[77:88] <- "Alpensüdseite"

  ### TEMPERATURE ###
  # check whether all or a large fraction of the data
  # are either above, below or in the range of the norm
  acurr_all <- vals$Abw[!is.na(vals$Abw)]
  a_ueber <- length(which(acurr_all > 0.5)) / length(acurr_all)
  a_unter <- length(which(acurr_all < -0.5)) / length(acurr_all)
  a_bereich <- 1 - a_ueber - a_unter
  quac <- quantile(acurr_all,probs = c(0.16,0.84))
  quac[quac>0] <- paste0("+",quac[quac>0])
                         
  # monthly mean temp ranks at stations
  vals$Rank_T <- rep(NA,length(vals$Station))
  vals$firstmeas_T <- rep(NA,length(vals$Station))
  
  for (s in 1:length(vals$Station)) {
    if (vals$Station[s]=="AND") {
      vals$Rank_T[s] <- NA
      vals$firstmeas_T[s] <- NA
    } else {
      recstat <- rekorde(top=10,minmax="max",year=bulletin$year,month=bulletin$month,station=vals$Station[s],parameter="ths200m0",rectype="m")
      vals$Rank_T[s] <- recstat$ranks_curryear
      vals$firstmeas_T[s] <- recstat$firstmeas
    }
  }

  diff_highlow <- abs(median(vals$Abw[vals$Hoehe>=1500],na.rm=T))-abs(median(vals$Abw[vals$Hoehe<1500],na.rm=T))

  # stations with time series of more than 100 years
  ranky100 <- vals$Rank_T[vals$firstmeas_T<(bulletin$year-100)]
  r1y100 <- which(vals$firstmeas_T<(bulletin$year-100) & vals$Rank_T==1)

  # greatest deviations in all regions
  regs <- unique(vals$Region)
  vhighest <- 1
  vlowest <- 1
  for (r in 1:length(regs)) {
    indr <- which(vals$Region == regs[r])
    vhighest[r] <- quantile(abs(vals$Abw[indr]),0.75,na.rm=TRUE)
    vlowest[r] <- quantile(abs(vals$Abw[indr]),0.25,na.rm=TRUE)
  }
  member_h <- cutree(hclust(dist(vhighest)),3)
  member_l <- cutree(hclust(dist(vlowest)),3)
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
  selhigh <- selhigh[order(match(selhigh$Abw, sort(selhigh$Abw,decreasing = TRUE))), ]
  selhigh <- selhigh[1:2,]
  sellow <- vals[which(vals$Region %in% regs[mlow]),]
  sellow <- sellow[order(match(sellow$Abw, sort(sellow$Abw,decreasing = FALSE))), ]
  sellow <- sellow[1:2,]
  selreg <- rbind(selhigh,sellow)
  sellow_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=sellow$Station)$station_name)
  selhigh_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=selhigh$Station)$station_name)

  # generate subset for a printable table
  # (reduced to the stations defined above)
  subset_climtab <- vals[which(vals$Station %in% stations),]
  subset_climtab <- subset_climtab[order(match(subset_climtab$Station, stations)), ]
  subset_climtab <- rbind(subset_climtab,selreg)
  subset_climtab <- subset_climtab[,c(1:5,16:17)]
  sn <- mchdwh::station_info(nat_abbr=subset_climtab$Station)
  sn <- sn[order(match(sn$nat_abbr, subset_climtab$Station)), ]
  subset_climtab$Station <- sn$station_name
  subset_climtab$Abw[subset_climtab$Abw > 0] <- paste0("+", subset_climtab$Abw[subset_climtab$Abw > 0])
  rownames(subset_climtab) <- NULL
  attributes(subset_climtab)$names <- c("Station","Höhe (m)","Monatsmittel (°C)","Norm (°C)","Abweichung (°C)","Rang","Messbeginn")
  
  ### PRECIPITATION ###
  # check whether all or a large fraction of the data
  # are either above, below or in the range of the norm
  acurr_all_prec <- vals$R.dev[!is.na(vals$R.dev)]
  a_ueber_prec <- length(which(acurr_all_prec > 105)) / length(acurr_all_prec)
  a_unter_prec <- length(which(acurr_all_prec < 95)) / length(acurr_all_prec)
  a_bereich_prec <- 1 - a_ueber_prec - a_unter_prec
  quac_prec <- quantile(acurr_all_prec,probs = c(0.16,0.84))
  # round values to next 5 for precip
  quac_prec <- round(quac_prec/5)*5
  
  # monthly prec sum ranks at stations
  vals$Rank_R_wet <- rep(NA,length(vals$Station))
  vals$Rank_R_dry <- rep(NA,length(vals$Station))
  vals$firstmeas_R <- rep(NA,length(vals$Station))
  
  for (s in 1:length(vals$Station)) {
    if (vals$Station[s] %in% c("AND","LAE","HOE","JUN","GSB","BEH")) {
      vals$Rank_R_wet[s] <- NA
      vals$Rank_R_dry[s] <- NA
      vals$firstmeas_R[s] <- NA
    } else {
      recstat_R <- rekorde(top=10,minmax="max",year=bulletin$year,month=bulletin$month,station=vals$Station[s],parameter="rhs150m0",rectype="m")
      vals$Rank_R_wet[s] <- recstat_R$ranks_curryear
      vals$Rank_R_dry[s] <- bulletin$year-recstat_R$firstmeas+2-recstat_R$ranks_curryear
      vals$firstmeas_R[s] <- recstat_R$firstmeas
    }
  }
  vals$Rank_R_comb <- ifelse(
    is.na(vals$Rank_R_wet) | is.na(vals$Rank_R_dry),
    "",
    ifelse (
      vals$Rank_R_wet <= vals$Rank_R_dry,
      paste0(vals$Rank_R_wet, "\u2193"),
      paste0(vals$Rank_R_dry, "\u2191")
    )
  )

  # stations with time series of more than 100 years
  ranky100_R_wet <- vals$Rank_R_wet[vals$firstmeas_R<(bulletin$year-100)]
  ranky100_R_dry <- vals$Rank_R_dry[vals$firstmeas_R<(bulletin$year-100)]
  r1y100_R_wet <- which(vals$firstmeas_R<(bulletin$year-100) & vals$Rank_R_wet==1)
  r1y100_R_dry <- which(vals$firstmeas_R<(bulletin$year-100) & vals$Rank_R_dry==1)
  
  # greatest deviations in all regions
  vhighest_R <- 1
  vlowest_R <- 1
  for (r in 1:length(regs)) {
    indr <- which(vals$Region == regs[r])
    vhighest_R[r] <- quantile(vals$R.dev[indr],0.75,na.rm=TRUE)
    vlowest_R[r] <- quantile(vals$R.dev[indr],0.25,na.rm=TRUE)
  }
  member_h_R <- cutree(hclust(dist(vhighest_R)),3)
  member_l_R <- cutree(hclust(dist(vlowest_R)),3)
  mr_h_R <- 0
  mr_l_R <- 0
  for (i in 1:3) {
    indm_h_R <- which(member_h_R == i)
    mr_h_R[i] <- mean(vhighest_R[indm_h_R])
    indm_l_R <- which(member_l_R == i)
    mr_l_R[i] <- mean(vlowest_R[indm_l_R])
  }
  mhigh_R <- which(mr_h_R==max(mr_h_R))
  mhigh_R <- which(member_h_R == mhigh_R)
  regshigh_R <- collapse_sentence(regs[mhigh_R])
  mlow_R <- which(mr_l_R==min(mr_l_R))
  mlow_R <- which(member_l_R == mlow_R)
  regslow_R <- collapse_sentence(regs[mlow_R])
  # add a few relevant stations to the list below
  selhigh_R <- vals[which(vals$Region %in% regs[mhigh_R]),]
  selhigh_R <- selhigh_R[order(match(selhigh_R$R.dev, sort(selhigh_R$R.dev,decreasing = TRUE))), ]
  selhigh_R <- selhigh_R[1:2,]
  sellow_R <- vals[which(vals$Region %in% regs[mlow_R]),]
  sellow_R <- sellow_R[order(match(sellow_R$R.dev, sort(sellow_R$R.dev,decreasing = FALSE))), ]
  sellow_R <- sellow_R[1:2,]
  selreg_R <- rbind(selhigh_R,sellow_R)
  sellow_stats_R <- collapse_sentence(mchdwh::station_info(nat_abbr=sellow_R$Station)$station_name)
  selhigh_stats_R <- collapse_sentence(mchdwh::station_info(nat_abbr=selhigh_R$Station)$station_name)
  
  # generate subset for a printable table
  # (reduced to the stations defined above)
  subset_climtab_R <- vals[which(vals$Station %in% stations),]
  subset_climtab_R <- subset_climtab_R[order(match(subset_climtab_R$Station, stations)), ]
  subset_climtab_R <- rbind(subset_climtab_R,selreg_R)
  subset_climtab_R <- subset_climtab_R[,c(1,11:13,21,20)]
  sn_R <- mchdwh::station_info(nat_abbr=subset_climtab_R$Station)
  sn_R <- sn_R[order(match(sn_R$nat_abbr, subset_climtab_R$Station)), ]
  subset_climtab_R$Station <- sn_R$station_name
  rownames(subset_climtab_R) <- NULL
  attributes(subset_climtab_R)$names <- c("Station","Monatssumme (mm)","Norm (mm)","% der Norm","Rang","Messbeginn")
  
  return(
    list (
      # temperature
      allvalues = acurr_all, anteil_ueber = a_ueber, anteil_unter = a_unter, anteil_bereich = a_bereich,
      quantiles = quac, numb_stats_high = mhigh, regshigh = regshigh, numb_stats_low = mlow, regslow = regslow,
      selhigh_stats = selhigh_stats, sellow_stats = sellow_stats, diff_highlow = diff_highlow, 
      ranky100 = ranky100, climtab_vals = vals, rank1_longseries = r1y100, subset_climtab = subset_climtab,
      # precipitation 
      allvalues_R = acurr_all_prec, anteil_ueber_R = a_ueber_prec, anteil_unter_R = a_unter_prec, 
      anteil_bereich_R = a_bereich_prec, quantiles_R = quac_prec, numb_stats_high_R = mhigh_R, 
      regshigh_R = regshigh_R, numb_stats_low_R = mlow_R, regslow_R = regslow_R, 
      selhigh_stats_R = selhigh_stats_R, sellow_stats_R = sellow_stats_R, 
      subset_climtab_R = subset_climtab_R, ranky100_R_wet = ranky100_R_wet, ranky100_R_dry = ranky100_R_dry,
      rank1_longseries_R_wet = r1y100_R_wet, rank1_longseries_R_dry = r1y100_R_dry
    )
  )

}