day_records <- function(ycurr, mon) {
  
  # Tagesrekorde
  dayrec_stats <- c("ABO","AIG","ALT","ARO","BAS","BEH","BER","BLA","BRL","BUF","BUS","CDF","CGI","CHA","CHD","CHM","CHU",
                    "CIM","COM","COV","DAV","DEM","DIS","DOL","EBK","EIN","ELM","ENG","EVO","FAH","FRE","GLA","GRA","GRC",
                    "GRH","GRO","GSB","GUE","GUT","GVE","HAI","HLL","HOE","INT","JUN","KLO","KOP","LAG","LUG","LUZ","MAG",
                    "MER","MLS","MVE","NAP","NEU","OTL","PAY","PIL","PIO","PLF","PUY","RAG","REH","ROB","ROE","RUE","SAE",
                    "SAM","SBE","SBO","SCU","SHA","SIA","SIO","SMA","SMM","STG","TAE","ULR","VAD","VIS","WAE","WFJ","WYN","ZER")
  Txx <- rep(NA,length(dayrec_stats))
  Rank_Txx <- rep(NA,length(dayrec_stats))
  firstmeas_Txx <- rep(NA,length(dayrec_stats))
  date_Txx <- rep(NA,length(dayrec_stats))
  for (s in 1:length(dayrec_stats)) {
    recstat <- rekorde(top=10,minmax="max",year=ycurr,month=mon,station=dayrec_stats[s],parameter="ths200dx",rectype="m")
    Rank_Txx[s] <- recstat$ranks_curryear[1]
    Txx[s] <- recstat$values_curryear[1]
    firstmeas_Txx[s] <- recstat$firstmeas
    date_Txx[s] <- paste0(substr(recstat$dates_curryear[1],7,8),".",substr(recstat$dates_curryear[1],5,6),".")
  }
  Txx_all <- data.frame(Txx,date_Txx,Rank_Txx,firstmeas_Txx)
  row.names(Txx_all) <- dayrec_stats
  Txx_sorted <- Txx_all[order(Txx_all$Rank_Txx), ]
  Txx_sorted_subset <- Txx_sorted[Txx_sorted$Rank_Txx <= 5 & Txx_sorted$firstmeas_Txx <= 1959, ]
  # number of new Txx records
  numrec_Txx <- dim(Txx_sorted_subset)[1]
  sinf_Txx <- mchdwh::station_info(nat_abbr=row.names(Txx_sorted_subset))
  row.names(Txx_sorted_subset) <- sinf_Txx$station_name[order(match(sinf_Txx$nat_abbr,row.names(Txx_sorted_subset)))]
  Txx_sorted_subset_pretty <- Txx_sorted_subset
  names(Txx_sorted_subset_pretty) <- c("Maximale Temperatur (°C)","Datum","Rang","Messbeginn")
  if (numrec_Txx>6) {
    Txx_sorted_subset_pretty <- Txx_sorted_subset_pretty[1:6,]
  }
  
  Tnx <- rep(NA,length(dayrec_stats))
  Rank_Tnx <- rep(NA,length(dayrec_stats))
  firstmeas_Tnx <- rep(NA,length(dayrec_stats))
  date_Tnx <- rep(NA,length(dayrec_stats))
  for (s in 1:length(dayrec_stats)) {
    recstat <- rekorde(top=10,minmax="max",year=ycurr,month=mon,station=dayrec_stats[s],parameter="ths200dn",rectype="m")
    Rank_Tnx[s] <- recstat$ranks_curryear[1]
    Tnx[s] <- recstat$values_curryear[1]
    firstmeas_Tnx[s] <- recstat$firstmeas
    date_Tnx[s] <- paste0(substr(recstat$dates_curryear[1],7,8),".",substr(recstat$dates_curryear[1],5,6),".")
  }
  Tnx_all <- data.frame(Tnx,date_Tnx,Rank_Tnx,firstmeas_Tnx)
  row.names(Tnx_all) <- dayrec_stats
  Tnx_sorted <- Tnx_all[order(Tnx_all$Rank_Tnx), ]
  Tnx_sorted_subset <- Tnx_sorted[Tnx_sorted$Rank_Tnx <= 5 & Tnx_sorted$firstmeas_Tnx <= 1959, ]
  # number of new Txx records
  numrec_Tnx <- dim(Tnx_sorted_subset)[1]
  sinf_Tnx <- mchdwh::station_info(nat_abbr=row.names(Tnx_sorted_subset))
  row.names(Tnx_sorted_subset) <- sinf_Tnx$station_name[order(match(sinf_Tnx$nat_abbr,row.names(Tnx_sorted_subset)))]
  Tnx_sorted_subset_pretty <- Tnx_sorted_subset
  names(Tnx_sorted_subset_pretty) <- c("Höchstes Tagesminimum (°C)","Datum","Rang","Messbeginn")
  if (numrec_Tnx>6) {
    Tnx_sorted_subset_pretty <- Tnx_sorted_subset_pretty[1:6,]
  }
  
  # statslow_north <- vals$Station[vals$Region %in% c("Westschweiz","Mittelland","Alpennordhang","Wallis") & vals$Hoehe < 500]
  # lownorth_Rank_Tmax <- rep(NA, length(statslow_north))
  # lownorth_Tmax <- rep(NA, length(statslow_north))
  # for (s in 1:length(statslow_north)) {
  #   recstat <- rekorde(top=10,minmax="max",year=2024,month=mon,station=statslow_north[s],parameter="ths200dx",rectype="m")
  #   lownorth_Rank_Tmax[s] <- recstat$ranks_curryear[1]
  #   lownorth_Tmax[s] <- recstat$values_curryear[1]
  # }
  # statslow_south <- vals$Station[vals$Region == "Alpensüdseite" & vals$Hoehe < 400]
  # lowsouth_Rank_Tmax <- rep(NA, length(statslow_south))
  # lowsouth_Tmax <- rep(NA, length(statslow_south))
  # for (s in 1:length(statslow_south)) {
  #   recstat <- rekorde(top=10,minmax="max",year=2024,month=mon,station=statslow_south[s],parameter="ths200dx",rectype="m")
  #   lowsouth_Rank_Tmax[s] <- recstat$ranks_curryear[1]
  #   lowsouth_Tmax[s] <- recstat$values_curryear[1]
  # }
  
  return(list(
    numrec_Txx = numrec_Txx,
    Txx_sorted_subset = Txx_sorted_subset,
    Txx_sorted_subset_pretty = Txx_sorted_subset_pretty,
    numrec_Tnx = numrec_Tnx,
    Tnx_sorted_subset = Tnx_sorted_subset,
    Tnx_sorted_subset_pretty = Tnx_sorted_subset_pretty
  ))
}


rekorde <- function(top=10,minmax="max",year=2024,month=2,station="SMA",parameter="ths200m0",rectype="m") {
  # Get records for different variables up to current month
  # Input
  if (rectype=="m") {
    data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                            param_short=parameter,
                                            year=c(1864,year),
                                            month=month,meas_cat=1),
                     error=function(e) {
                       # try meas_cat 12
                       mchdwh::dwhget_surface(nat_abbr=station,
                                              param_short=parameter,
                                              year=c(1864,year),
                                              month=month,meas_cat=12)
                     })
  }
  
  if (rectype=="y") {
    data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                            param_short=parameter,
                                            year=c(1864,year),meas_cat=1),
                     error=function(e) {
                       # try meas_cat 12
                       mchdwh::dwhget_surface(nat_abbr=station,
                                              param_short=parameter,
                                              year=c(1864,year),meas_cat=12)
                     })
  }
  
  
  if (minmax == "max") {
    dsort <- order(data$value,as.numeric(data$datetime),decreasing=T)
  } else {
    dsort <- order(data$value,as.numeric(data$datetime),decreasing=F)
  }
  vals <- data$value[dsort]
  ranks <- 1
  for (i in 2:length(vals)) {
    if (vals[i]-vals[i-1] != 0) {
      ranks[i] <- i
    } else {
      ranks[i] <- ranks[i-1]
    }
  }
  
  ind_recs <- which(ranks <= top)
  if (rectype=="m") {
    ind_curr <- which(dsort %in% which(as.numeric(substr(data$datetime,1,4))==year))
  } else {
    ind_curr <- which(dsort %in% which(as.numeric(substr(data$datetime,1,4))==year & as.numeric(substr(data$datetime,5,6))==month))
  }
  
  record <- list()
  record$datetime <- data$datetime[dsort][ind_recs]
  record$value <- data$value[dsort][ind_recs]
  record$ranks <- ranks[ind_recs]
  record$firstmeas <- as.numeric(substr(data$datetime[1],1,4))
  
  if (length(ind_curr)>=1) {
    ranks_currentyear <- ranks[ind_curr]
    dates_currentyear <- data$datetime[dsort][ind_curr]
    values_currentyear <- data$value[dsort][ind_curr]
    record$dates_curryear <- dates_currentyear
    record$values_curryear <- values_currentyear
    record$ranks_curryear <- ranks_currentyear
  }
  
  return(record)
  
}

