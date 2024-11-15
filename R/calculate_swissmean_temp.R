calculate_swissmean_temp <- function (bulletin) {
  
  log_info("Calculating swissmean for temperature")
  
  cache_file <- file.path(bulletin$cache_path, "swissmean_temp.Rdata")
  if (file.exists(cache_file)) {
    log_debug("... from cache")
    return(readRDS(cache_file))
  }
  
  # Download data
  filename_abs <- download_monatsbilanz_temp(bulletin, valueBase = "abs", provisional = bulletin$provisional, filename = "monatsbilanz_temp_abs.txt")
  data_abs <- read.table(filename_abs, header = TRUE)
  
  filename_anom <- download_monatsbilanz_temp(bulletin, valueBase = "anom", provisional = bulletin$provisional, filename = "monatsbilanz_temp_anom.txt")
  data_anom <- read.table(filename_anom, header = TRUE)
  
  # absolute temperature, swissmean
  year <- data_abs$year
  poscurr <- which(year == bulletin$year)
  ycurr <- year[poscurr]
  ybeg <- year[1]
  abs  <- data_abs$val
  vcurr <- round(abs[poscurr],1)
  vcurr_t <- format(vcurr, nsmall=1)
  
  # anomaly temperature, swissmean
  anom <- data_anom$val
  acurr <- round(anom[poscurr],1)
  acurr_t <- format(acurr, nsmall=1)
  if (acurr_t > 0) {
    acurr_t <- paste0("+", acurr_t)
  }
  
  # rank swissmean    
  ranking <- sort.int(anom,decreasing=T,index.return=T)
  rankcurr <- which(ranking$ix==poscurr)
  
  if (rankcurr != 1) {
    ind01 <- ranking$ix[1]
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
  } else {
    ind01 <- ranking$ix[2]
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
  }
  reca_t <- format(reca, nsmall=1)
  if (reca_t > 0) {
    reca_t <- paste0("+", reca_t)
  }
  recval_t <- format(recval, nsmall=1)
  
  # years similar to current
  diffc_t5 <- abs(ranking$x[which(ranking$ix==poscurr)]-ranking$x[1:5])
  if (any(diffc_t5<0.1)) {
    isim <- which(diffc_t5<0.1)
    isimy <- ranking$ix[isim]
    isimy <- isimy[-which(isimy==poscurr)]
  } else {
    isimy <- NULL
  }
  
  loess <- evoclim::loess.filt.knmi(x=abs,years=year,y1=1885,y2=ycurr,y1asmean=TRUE)
  signif <- as.numeric(loess$incr.pval)
  diff <- round(as.numeric(c(loess$conf.l[poscurr]-loess$val1,loess$t.incr,loess$conf.u[poscurr]-loess$val1)),1)
  ydiff_ca <- round(ycurr-1885+1,-1)
  
  resid <- as.numeric(quantile(abs-loess$fit,probs=c(0.16,0.84)))
  
  bounds <- format(round(c(loess$val2+resid[1],loess$val2+resid[2]),1), nsmall=1)
  
  swissmean_temp <- list(
    curr_temp = vcurr_t, curr_temp_dev = acurr_t, 
    curr_rank = rankcurr, meas_start = ybeg,
    ind_simyears = isimy, year_form_rec = recy,
    val_form_rec = recval_t, dev_form_rec = reca_t,
    all_years = year, loess = loess, signif = signif,
    climate_change_signal = diff,
    y_since_preind = ydiff_ca, loess_bounds = bounds
  )
  
  # cache the results
  saveRDS(swissmean_temp, cache_file)
  
  swissmean_temp
}