
# Input
bulletin2 <- list(month = 8, year = 2024)

# Computation of all interesting daily records
parameters <- list()
parameters$names <- c("ths20d0x","ths20d0n","ths20dxx","ths20dxn","ths20dnx","ths20dnn","rre15d0x","fu301d1x")
parameters$desc_de <- c("Höchste Tagesmitteltemperatur", "Tiefste Tagesmitteltemperatur", 
                        "Höchste Tagesmaximumtemperatur", "Tiefste Tagesmaximumtemperatur",
                        "Höchste Tagesminimumtemperatur", "Tiefste Tagesminimumtemperatur",
                        "Höchste Tagessumme des Niederschlags", "Höchste Windspitze")

sentence01 <- NULL
sentence02 <- NULL
c <- 0
for (p in 1:length(parameters$names)) {
  rec_data <- NULL
  result <- tryCatch(
    {
      rec_data <- process_extreme_values(param_short = parameters$names[p], bulletin = bulletin2)
    }, 
    error = function(e) {
      message("Keine Rekorde in diesem Monat zu diesem Parameter: ", e$message)
      return(NULL)
    }
  )
  if (rec_data$rec_avail) {
    c <- c + 1
    if (c == 1) {
      sentence01 <- "Folgende Tagesrekorde an Stationen mit langen Messreihen wurden im vergangenen Monat gemessen:"
      sentence02 <- paste0(parameters$desc_de[p], ", Rang ",rec_data$highest_rank, ": ", rec_data$station_record_info)
    } else {
      sentence02 <- c(sentence02, 
                      paste0(parameters$desc_de[p], ", Rang ",rec_data$highest_rank, ": ", rec_data$station_record_info))
    }
  }
}



