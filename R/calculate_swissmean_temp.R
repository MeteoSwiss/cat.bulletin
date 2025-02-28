calculate_swissmean_temp <- function (bulletin) {
  
  log_info("Calculating swissmean for temperature")
  
  cache_file <- file.path(bulletin$cache_path, "swissmean_temp.Rdata")
  if (file.exists(cache_file)) {
    log_debug("... from cache")
    return(readRDS(cache_file))
  }
  
  # Download data
  filename_abs <- download_monatsbilanz_temp(bulletin, valueBase = "abs", provisional = bulletin$provisional, filename = "monatsbilanz_temp_abs.txt")
  # Test other months
  #mshort <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  #filename_abs <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", paste0("ths200m0.swissmean.m.",mshort[bulletin$month],".1864.",bulletin$year,".abs.txt"), package = "cat.bulletin")
  #
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
  acurr_t <- sprintf("%+.1f",acurr)

  # rank swissmean    
  rankcurr <- data_abs$rank.h[poscurr]

  # uncertainties from outlook (bulletin$provisional == TRUE)
  if (bulletin$provisional) {
    outl <- mmtpred::mmtpred(station = "swissmean", granul = "m", out.type = "val", include.extr = TRUE, ranking = "high")
    outl_dev <- mmtpred::mmtpred(station = "swissmean", granul = "m", out.type = "val", include.extr = TRUE, ranking = "high", begin.normp = "1991", end.normp = "2020", write.dev = TRUE)
    abs_uncertainty <- c(outl[["2.5%"]], outl[["97.5%"]])
    dev_uncertainty <- c(outl_dev[["2.5%"]], outl_dev[["97.5%"]])
    dev_uncertainty <- sapply(dev_uncertainty, function(x) {
      sign <- if (x > 0) "+" else ""
      sprintf("%s%.1f", sign, x)
    })
    abs_diff_neq_0 <- !all(abs_uncertainty==abs_uncertainty[1])
    rank_uncertainty <- c(attributes(outl)$ranks[["97.5%"]], attributes(outl)$ranks[["2.5%"]])
    rank_diff_neq_0 <- !all(rank_uncertainty==rank_uncertainty[1])
    fcst_delay <- attributes(outl)$dd.fcst.delay
  }

  if (rankcurr != 1) {
    ind01 <- which(data_abs$rank.h==1)
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
  } else {
    ind01 <- which(data_abs$rank.h==2)
    recy <- year[ind01]
    recval <- round(abs[ind01],1)
    reca <- round(anom[ind01],1)
  }
  reca_t <- sprintf("%+.1f",reca)
  recval_t <- format(recval, nsmall=1)
  
  # years similar to current among the 5 warmest years
  diffc_t5 <- abs(abs[poscurr] - abs)
  if (any(diffc_t5<0.09)) {
    isimy <- which(diffc_t5<0.09)
    isimy <- isimy[-which(isimy==poscurr)]
  } else {
    isimy <- NULL
  }
  
  loess <- evoclim::loess.filt.knmi(x=abs,years=year,y1=1885,y2=ycurr,y1asmean=TRUE)
  signif <- as.numeric(loess$incr.pval)
  diff <- round(c(loess$incr.cf[1],loess$t.incr,loess$incr.cf[2]),1)
  ydiff_ca <- round(ycurr-1885+1,-1)
  
  resid1 <- as.numeric(quantile(abs-loess$fit,probs=c(0.16,0.84)))
  resid2 <- as.numeric(quantile(abs-loess$fit,probs=c(0.025,0.975)))
  
  bounds1 <- format(round(c(loess$val2+resid1[1],loess$val2+resid1[2]),1), nsmall=1)
  bounds2 <- format(round(c(loess$val2+resid2[1],loess$val2+resid2[2]),1), nsmall=1)
  
  swissmean_temp <- list(
    curr_temp = vcurr_t, curr_temp_dev = acurr_t, 
    curr_rank = rankcurr, meas_start = ybeg,
    ind_simyears = isimy, year_form_rec = recy,
    val_form_rec = recval_t, dev_form_rec = reca_t,
    all_years = year, loess = loess, signif = signif,
    climate_change_signal = diff,
    y_since_preind = ydiff_ca, loess_bounds1 = bounds1,
    loess_bounds2 = bounds2
  )
  if (bulletin$provisional) {
    swissmean_temp <- c(swissmean_temp, list(
                        abs_uncertainty = abs_uncertainty, rank_uncertainty = rank_uncertainty,
                        fcst_delay = fcst_delay, abs_diff_neq_0 = abs_diff_neq_0, 
                        rank_diff_neq_0 = rank_diff_neq_0, dev_uncertainty = dev_uncertainty))
  }
  
  # cache the results
  saveRDS(swissmean_temp, cache_file)
  
  swissmean_temp
}