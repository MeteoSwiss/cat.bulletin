rekorde <- function(top=10,minmax="max",year=2024,month=2,station="SMA",parameter="ths200m0",rectype="m") {
# Get records for different variables up to current month
# Input
if (rectype=="m") {
  data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                          param_short=parameter,
                                          year=c(1864,year),
                                          month=month,meas_cat=1),
                   error=function(e) e)
  if (is(data,"error")) {
    data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                            param_short=parameter,
                                            year=c(1864,year),
                                            month=month,meas_cat=12),
                     error=function(e) e)
  }
}
  if (rectype=="y") {
    data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                            param_short=parameter,
                                            year=c(1864,year),meas_cat=1),
                     error=function(e) e)
    if (is(data,"error")) {
      data <- tryCatch(mchdwh::dwhget_surface(nat_abbr=station,
                                              param_short=parameter,
                                              year=c(1864,year),meas_cat=12),
                       error=function(e) e)
    }
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

} #EOF
