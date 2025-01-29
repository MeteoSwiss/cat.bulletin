library(mchdwh)

df  <- mchdwh::dwhget_extreme_values(param_short = "ths20m0x", ref_period_id = 1, date_range_id = 8, year = 2024)
df2 <- mchdwh::dwhget_extreme_values(param_short = "ths20m0x", ref_period_id = 1, date_range_id = 8, ranking = 2)
df1 <- mchdwh::dwhget_extreme_values(param_short = "ths20m0x", ref_period_id = 1, date_range_id = 8, ranking = 1)

# Define the ranks to consider
ranks <- 1:10

# Create the rank summary and group stations and temperatures by rank
rank_summary <- table(factor(df$ranking, levels = ranks))
stations_by_rank <- lapply(ranks, function(r) {
  df$nat_abbr[df$ranking == r]
})
temperatures_by_rank <- lapply(ranks, function(r) {
  df$value[df$ranking == r]
})
names(stations_by_rank) <- ranks  # Assign numeric rank names directly
names(temperatures_by_rank) <- ranks

# Generate German sentence for the highest rank with adjusted phrasing
generate_highest_rank_sentence <- function(rank_summary, stations_by_rank, temperatures_by_rank, df2, df1) {
  # Find the highest (smallest) rank with at least one station
  highest_rank <- min(as.numeric(names(rank_summary)[rank_summary > 0]), na.rm = TRUE)

  # Get the count, stations, and temperatures for the highest rank
  count <- rank_summary[[as.character(highest_rank)]]
  stations <- stations_by_rank[[as.character(highest_rank)]]
  temperatures <- temperatures_by_rank[[as.character(highest_rank)]]
  
  # Determine singular or plural form
  station_word <- ifelse(count == 1, "Messstation", "Messstationen")
  
  # Generate the sentence
  if (count <= 5) {
    # Combine stations with their temperatures
    station_with_temps <- paste0(mchdwh::station_info(nat_abbr=stations)$station_name[order(mchdwh::station_info(nat_abbr=stations)$nat_abbr,stations)],
                                 " (", sprintf("%.1f", temperatures), " °C)")
    station_names <- paste(station_with_temps, collapse = ", ")
    
    # Add information about previous records from df2 (new rank 2) or df1 (rank 1 from previous year still valid)
    if (highest_rank == 1) {
      previous_records <- df2[df2$nat_abbr %in% stations, ]
    } else {
      previous_records <- df1[df1$nat_abbr %in% stations, ]
    }
    sw <- ifelse(highest_rank == 1, "waren", "sind")
    prevrec_stat_names <- mchdwh::station_info(nat_abbr=previous_records$nat_abbr)$station_name[order(mchdwh::station_info(nat_abbr=previous_records$nat_abbr)$nat_abbr,previous_records$nat_abbr)]
    shortest_period <- as.numeric(substr(previous_records$till_date,1,4)) - 
      as.numeric(substr(previous_records$min_since_date,1,4)) + 1
    shortest_period <- trunc(shortest_period/10)*10
    shortest_period <- min(shortest_period)
    previous_record_info <- paste0(
      prevrec_stat_names, 
      " (", 
      sprintf("%.1f", previous_records$value), 
      " °C, ", 
      substr(previous_records$datetime, 1, 4), 
      ")"
    )
    previous_record_sentence <- sprintf(
      "Die bisherigen Monatsrekorde an diesen Messstationen %s: %s.", 
      sw, paste(previous_record_info, collapse = ", ")
    )
    
    sentence <- sprintf(
      "Die Monatsmitteltemperatur im August 2024 erreicht an %d %s mit Messreihen von über %i Jahren den %d. Rang: %s. %s", 
      count, station_word, shortest_period, highest_rank, station_names, previous_record_sentence
    )
  } else {
    # Only mention the count
    sentence <- sprintf(
      "Die Monatsmitteltemperatur im August 2024 erreicht an %d %s mit Messreihen von über %i Jahren den %d. Rang.", 
      count, station_word, shortest_period, highest_rank
    )
  }
  
  return(sentence)
}

# Generate and print the sentence for the highest rank
sentence <- generate_highest_rank_sentence(rank_summary, stations_by_rank, temperatures_by_rank, df2, df1)
cat(sentence)

