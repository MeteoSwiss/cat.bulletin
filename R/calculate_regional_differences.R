calculate_regional_differences <- function(bulletin) {
  
  log_info("Calculating regional differences")
  
  cache_file <- file.path(bulletin$cache_path, "regional_differences.Rdata")
  if (file.exists(cache_file)) {
    log_debug("... from cache")
    return(readRDS(cache_file))
  }
  
  # prepare climtable
  mondate <- formatC(bulletin$month,width=2, flag="0")
  begdate <- paste0(bulletin$year,mondate,"01")
  enddate <- paste0(bulletin$year,mondate,datefuns::days.of.mon(bulletin$year, bulletin$month))
  
  data <- clim.table::climtable(period=c(begdate,enddate), outDir = bulletin$data_path)
  
  # set region every station is belonging to
  regsort <- c("Jura","Mittelland","Alpennordhang","Wallis","Nord- und Mittelbünden","Engadin","Alpensüdseite")
  vals <- data$dana$vals

  station_info <- mchdwh::station_info(nat_abbr = vals$Station, region_type_id = 1)
  rownames(station_info) <- station_info$nat_abbr
  vals$Region <- station_info[vals$Station, "region_name_G"]
  vals$Region <- clean_region_names(vals$Region)
  assert_that(all(vals$Region %in% regsort), msg = "The regsort vector does not correspond to region values of the stations")

  ### TEMPERATURE ###
  regdiff_T <- comp_regdiff(parameter = "T", vals = vals, regsort = regsort)
  subset_climtab_T <- regdiff_T$subset_climtab

  # Local temperature rankin
  high_temp_records <- process_extreme_values(param_short = "ths20m0x", bulletin)
  low_temp_records  <- process_extreme_values(param_short = "ths20m0n", bulletin)

  ### PRECIPITATION ###
  regdiff_P <- comp_regdiff(parameter = "P", vals = vals, regsort = regsort)
  subset_climtab_P <- regdiff_P$subset_climtab
  # subset_climtab_P <- flextable::flextable(regdiff_P$subset_climtab)
  # subset_climtab_P <- flextable::set_caption(subset_climtab_P, caption = paste0("Monatsniederschläge für den ",month_str(bulletin$month)," an ausgewählten Stationen im Messnetz von MeteoSchweiz. Es ist die aktuelle Monatssumme, der Referenzwert (1991-2020) und das Verhältnis zur Referenzperiode in % angegeben."))
  
  # Local precipitation ranking
  high_prec_records <- process_extreme_values(param_short = "rhs15m0x", bulletin)
  low_prec_records  <- process_extreme_values(param_short = "rhs15m0n", bulletin)

  ### SUNSHINE DURATION ###
  regdiff_S <- comp_regdiff(parameter = "S", vals = vals, regsort = regsort)
  subset_climtab_S <- regdiff_S$subset_climtab
  # subset_climtab_S <- flextable::flextable(regdiff_S$subset_climtab)
  # subset_climtab_S <- flextable::set_caption(subset_climtab_S, caption = paste0("Monatliche Sonnenscheindauer im ",month_str(bulletin$month)," ",bulletin$year," an ausgewählten Stationen von MeteoSchweiz.  Es ist die aktuelle Monatssumme, der Referenzwert (1991-2020) und das Verhältnis zur Referenzperiode in % angegeben."))
  
  # Local sunshine duration ranking
  high_sun_records <- process_extreme_values(param_short = "sh200m0x", bulletin)
  low_sun_records  <- process_extreme_values(param_short = "sh200m0n", bulletin)
  
  # Output
  regional_differences <- list(
    # temperature
    allvalues = regdiff_T$allvalues, anteil_ueber = regdiff_T$anteil_ueber, anteil_unter = regdiff_T$anteil_unter, 
    anteil_bereich = regdiff_T$anteil_bereich, quantiles = regdiff_T$quantiles, 
    numb_stats_high = regdiff_T$numb_stats_high, regshigh = regdiff_T$regshigh, numb_stats_low = regdiff_T$numb_stats_low, 
    regslow = regdiff_T$regslow, selhigh_stats = regdiff_T$selhigh_stats, sellow_stats = regdiff_T$sellow_stats, 
    selhigh_abw = regdiff_T$selhigh_abw, sellow_abw = regdiff_T$sellow_abw, diff_highlow = regdiff_T$diff_highlow, 
    climtab_vals = regdiff_T$climtab_vals, subset_climtab_T = subset_climtab_T, 
    high_temp_rec_avail = high_temp_records$rec_avail, low_temp_rec_avail = low_temp_records$rec_avail, 
    # precipitation 
    allvalues_P = regdiff_P$allvalues, anteil_ueber_P = regdiff_P$anteil_ueber, anteil_unter_P = regdiff_P$anteil_unter, 
    anteil_bereich_P = regdiff_P$anteil_bereich, quantiles_P = regdiff_P$quantiles, 
    numb_stats_high_P = regdiff_P$numb_stats_high, regshigh_P = regdiff_P$regshigh, numb_stats_low_P = regdiff_P$numb_stats_low, 
    regslow_P = regdiff_P$regslow, selhigh_stats_P = regdiff_P$selhigh_stats, sellow_stats_P = regdiff_P$sellow_stats, 
    selhigh_abw_P = regdiff_P$selhigh_abw, sellow_abw_P = regdiff_P$sellow_abw, 
    climtab_vals_P = regdiff_P$climtab_vals, subset_climtab_P = subset_climtab_P,
    high_prec_rec_avail = high_prec_records$rec_avail, low_prec_rec_avail = low_prec_records$rec_avail,
    # sunshine 
    allvalues_S = regdiff_S$allvalues, anteil_ueber_S = regdiff_S$anteil_ueber, anteil_unter_S = regdiff_S$anteil_unter, 
    anteil_bereich_S = regdiff_S$anteil_bereich, quantiles_S = regdiff_S$quantiles, 
    numb_stats_high_S = regdiff_S$numb_stats_high, regshigh_S = regdiff_S$regshigh, numb_stats_low_S = regdiff_S$numb_stats_low, 
    regslow_S = regdiff_S$regslow, selhigh_stats_S = regdiff_S$selhigh_stats, sellow_stats_S = regdiff_S$sellow_stats, 
    selhigh_abw_S = regdiff_S$selhigh_abw, sellow_abw_S = regdiff_S$sellow_abw, 
    climtab_vals_S = regdiff_S$climtab_vals, subset_climtab_S = subset_climtab_S,
    high_sun_rec_avail = high_sun_records$rec_avail, low_sun_rec_avail = low_sun_records$rec_avail
  )
  if (high_temp_records$rec_avail) {
    regional_differences <- c(regional_differences,
                              high_temp_highest_rank = high_temp_records$highest_rank, 
                              high_temp_count_hr = high_temp_records$count_hr, 
                              high_temp_shortest_period = high_temp_records$shortest_period, 
                              high_temp_station_record_info = high_temp_records$station_record_info)
  } 
  if (low_temp_records$rec_avail) {
    regional_differences <- c(regional_differences,
                              low_temp_highest_rank = low_temp_records$highest_rank, 
                              low_temp_count_hr = low_temp_records$count_hr,
                              low_temp_shortest_period = low_temp_records$shortest_period, 
                              low_temp_station_record_info = low_temp_records$station_record_info)
  } 
  if (high_prec_records$rec_avail) {
    regional_differences <- c(regional_differences,
                              high_prec_highest_rank = high_prec_records$highest_rank, 
                              high_prec_count_hr = high_prec_records$count_hr, 
                              high_prec_shortest_period = high_prec_records$shortest_period, 
                              high_prec_station_record_info = high_prec_records$station_record_info)
  } 
  if (low_prec_records$rec_avail) {
    regional_differences <- c(regional_differences,
                              low_prec_highest_rank = low_prec_records$highest_rank, 
                              low_prec_count_hr = low_prec_records$count_hr,
                              low_prec_shortest_period = low_prec_records$shortest_period, 
                              low_prec_station_record_info = low_prec_records$station_record_info)
  } 
  if (high_sun_records$rec_avail) {
    regional_differences <- c(regional_differences,
                              high_sun_highest_rank = high_sun_records$highest_rank, 
                              high_sun_count_hr = high_sun_records$count_hr, 
                              high_sun_shortest_period = high_sun_records$shortest_period, 
                              high_sun_station_record_info = high_sun_records$station_record_info)
  } 
  if (low_sun_records$rec_avail) {
    regional_differences <- c(regional_differences,
                              low_sun_highest_rank = low_sun_records$highest_rank, 
                              low_sun_count_hr = low_sun_records$count_hr, 
                              low_sun_shortest_period = low_sun_records$shortest_period, 
                              low_sun_station_record_info = low_sun_records$station_record_info)
  } 
  # cache the results
  saveRDS(regional_differences, cache_file)
  
  regional_differences
}

process_extreme_values <- function(param_short, bulletin) {
  # Initialize output parameters
  highest_rank <- NA
  count_hr <- NA
  shortest_period <- NA
  station_record_info <- NA
  
  unit <- mchdwh::param_info(param_short = param_short)$unit

  df <- NULL
  result <- tryCatch(
    {
      df <- mchdwh::dwhget_extreme_values(
        param_short = param_short, 
        ref_period_id = 1, 
        date_range_id = bulletin$month, 
        year = bulletin$year
      )
    }, 
    error = function(e) {
      message(paste0(param_short,": "), e$message)
      return(NULL)
    }
  )
  # Proceed if data is available
  if (!is.null(df)) {
    df2 <- mchdwh::dwhget_extreme_values(param_short = param_short, ref_period_id = 1, date_range_id = bulletin$month, ranking = 2)
    df1 <- mchdwh::dwhget_extreme_values(param_short = param_short, ref_period_id = 1, date_range_id = bulletin$month, ranking = 1)
    
    # Define ranks
    ranks_rec <- 1:10
    
    # Summarize ranks
    rank_summary <- table(factor(df$ranking, levels = ranks_rec))
    stations_by_rank <- lapply(ranks_rec, function(r) {
      df$nat_abbr[df$ranking == r]
    })
    values_by_rank <- lapply(ranks_rec, function(r) {
      df$value[df$ranking == r]
    })
    names(stations_by_rank) <- ranks_rec
    names(values_by_rank) <- ranks_rec
    
    # Determine the highest (smallest) rank
    highest_rank <- min(as.numeric(names(rank_summary)[rank_summary > 0]), na.rm = TRUE)
    
    # Get the count, stations, and values for the highest rank
    count_hr <- rank_summary[[as.character(highest_rank)]]
    stations_longseries <- stations_by_rank[[as.character(highest_rank)]]
    values_longseries <- values_by_rank[[as.character(highest_rank)]]
    
    # Combine stations with their values
    station_with_vals <- paste0(
      mchdwh::station_info(nat_abbr = stations_longseries)$station_name[
        order(mchdwh::station_info(nat_abbr = stations_longseries)$nat_abbr, stations_longseries)
      ], " ", sprintf("%.1f", values_longseries), "\u00A0", unit)

    # Add information about previous records
    if (highest_rank == 1) {
      previous_records <- df2[df2$nat_abbr %in% stations_longseries, ]
      word_record <- " (bisheriger Rekord: "
    } else {
      previous_records <- df1[df1$nat_abbr %in% stations_longseries, ]
      word_record <- " (Rekord: "
    }
    shortest_period <- as.numeric(substr(previous_records$till_date, 1, 4)) - 
      as.numeric(substr(previous_records$min_since_date, 1, 4)) + 1
    shortest_period <- trunc(shortest_period / 10) * 10
    shortest_period <- min(shortest_period)
    previous_record_info <- paste0(word_record, sprintf("%.1f", previous_records$value), "\u00A0", unit, ", ", 
                                   substr(previous_records$datetime, 1, 4), ")")
    
    station_record_info <- collapse_sentence(paste0(station_with_vals, previous_record_info), "de")
  }
  
  rec_avail <- FALSE
  # Return results
  if (!is.null(df)) {
    rec_avail <- TRUE
    return(list(
      rec_avail = rec_avail, 
      highest_rank = highest_rank,
      count_hr = count_hr,
      shortest_period = shortest_period,
      station_record_info = station_record_info
    ))
  } else {
    return(list(rec_avail = rec_avail))
  }
}

# Computation of regional differences in temperature, precipitation and sunshine duration
comp_regdiff <- function(parameter, vals, regsort) {
  
  # Fixed input, parameter-independent
  dev_probs <- c(0.16, 0.84)
  alt_limit <- 1500
  standard_stations <- c("BER","SMA","GVE","BAS","ENG","DAV","SIO","LUG","SAM")
  
  if (parameter == "T") {
    # Fixed input for temperature
    deviations <- "Abw"
    norm_range <- c(-0.5,0.5)
    tab_columns <- c(1:5)
  }
  if (parameter == "P") {
    # Fixed input for precipitation
    deviations <- "R.dev"
    norm_range <- c(95,105)
    tab_columns <- c(1,2,11:13)
  }
  if (parameter == "S") {
    # Fixed input for sunshine duration
    deviations <- "S.dev"
    norm_range <- c(95,105)
    tab_columns <- c(1,2,7:9)
  }
  
  acurr_all <- vals[[deviations]][!is.na(vals[[deviations]])]
  a_ueber <- length(which(acurr_all > norm_range[2])) / length(acurr_all)
  a_unter <- length(which(acurr_all < norm_range[1])) / length(acurr_all)
  a_bereich <- 1 - a_ueber - a_unter
  quac <- stats::quantile(acurr_all,probs = dev_probs)
  if (parameter == "T") {
    quac <- round(quac, digits=1)
    quac <- sprintf("%+.1f",quac)
  } else {
    quac_test <- round(quac/10, digits=0)*10
    # use values rounded to the nearest 10 if these values are not equal
    # for both quantiles, otherwise use values rounded to nearest integer
    if (all(quac_test==quac_test[1])) {
      quac <- round(quac, digits=0)
    } else {
      quac <- quac_test
    }
  }
  
  # temperature difference with altitude
  diff_highlow <- abs(stats::median(vals[[deviations]][vals$Hoehe>=alt_limit],na.rm=T))-abs(stats::median(vals[[deviations]][vals$Hoehe<alt_limit],na.rm=T))
  
  # greatest vals[[deviations]] in all regions
  regs <- unique(vals$Region)
  vhighest <- 1
  vlowest <- 1
  for (r in 1:length(regs)) {
    indr <- which(vals$Region == regs[r])
    vhighest[r] <- stats::quantile(abs(vals[[deviations]][indr]),0.75,na.rm=TRUE)
    vlowest[r] <- stats::quantile(abs(vals[[deviations]][indr]),0.25,na.rm=TRUE)
  }
  member_h <- stats::cutree(stats::hclust(stats::dist(vhighest)),3)
  member_l <- stats::cutree(stats::hclust(stats::dist(vlowest)),3)
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
  regshigh <- collapse_sentence(regs[mhigh], "de")
  mlow <- which(mr_l==min(mr_l))
  mlow <- which(member_l == mlow)
  regslow <- collapse_sentence(regs[mlow], "de")
  # add a few relevant stations to the list below
  selhigh <- vals[which(vals$Region %in% regs[mhigh]),]
  selhigh <- selhigh[order(match(selhigh[[deviations]], sort(selhigh[[deviations]],decreasing = TRUE))), ]
  selhigh <- selhigh[1:2,]
  sellow <- vals[which(vals$Region %in% regs[mlow]),]
  sellow <- sellow[order(match(sellow[[deviations]], sort(sellow[[deviations]],decreasing = FALSE))), ]
  sellow <- sellow[1:2,]
  sellow_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=sellow$Station)$station_name, "de")
  selhigh_stats <- collapse_sentence(mchdwh::station_info(nat_abbr=selhigh$Station)$station_name, "de")
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
    subset_climtab[[deviations]] <- sprintf("%+.1f", subset_climtab[[deviations]])
  }
  rownames(subset_climtab) <- NULL

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