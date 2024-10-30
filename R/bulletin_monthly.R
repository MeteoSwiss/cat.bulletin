#' Create the monthly bulletin
#' @param year Bulletin year
#' @param month Bulletin month
#' @param provisional boolean indicating if the provisional version of the bulletin shall be created
#' @param ... further general bulletin arguments forwarded to the create_bulletin function. Use them to set working directory etc. 
#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function(year = 2024, month = 8, provisional = FALSE, ...) {

  bulletin <- create_bulletin(bulletin_args = list(year = year,
                                                   month = month,
                                                   provisional = provisional),
                              ...)
  
  swissmean <- calculate_swissmean_temp(bulletin)
  regdiff <- calculate_regional_differences(bulletin)

  bulletin <- bulletin %>%
    monatsbulletin_head() %>%
    monatsbilanz_temp(swissmean = swissmean, regdiff = regdiff) %>%
    monatsbilanz_precip(regdiff = regdiff) %>%
    monatsbilanz_sun() %>%
    temporal_evolution(swissmean = swissmean) %>%
    monatsbulletin_daily_timeseries() %>%
    monatsbulletin_disclaimer() %>%
    monatsbulletin_more_info()
  
  #bulletin_pdfxmlzip(bulletin)
  bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
}

monatsbulletin_head <- function(bulletin) {
  
  lang <- "G"
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  add_text(bulletin, paste("# Klimabulletin", month[bulletin$month], bulletin$year)) %>%
  add_text(paste("Im Leadtext Reihenfolge der zu nennenden Parameter über die Ränge entscheiden. Super wären Sätze im Sinne von DER AUGUST 2024 WAR GEPRÄGT VON HOHEN TEMPERATUREN UND WENIG NIEDERSCHLAG."))

  basepath <- "/prod/zue/climate/basic_serv/information/klimabulletin/klimabulletin_automatisch/"
  
  if (bulletin$month < 10) { 
    monpath <- paste0("0",bulletin$month)
  } else {
    monpath <- bulletin$month
  }
  monpath <- paste0(bulletin$year,monpath)
  teasertext <- readLines(paste0(basepath,monpath,"/",monpath,"_teaser_text.txt"), n = 1)
  
  add_image(bulletin, filename = paste0(monpath,"_teaser_image.jpg"), filepath = paste0(basepath,monpath,"/",monpath,"_teaser_image.jpg"), caption=teasertext)

}

monatsbilanz_temp <- function(bulletin, swissmean, regdiff) {
  
  #input aus anaperiod
  mon = bulletin$month
  provisional = bulletin$provisional
  lang <- "G"
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  if (lang != "G"){
    month <- sapply(month,add_article)
    month <- as.character(month)
  }
  
  # # daily records
  # daily_records = day_records(ycurr = ycurr, mon = mon)
  # 
  # numrec_Txx = daily_records$numrec_Txx
  # Txx_sorted_subset = daily_records$Txx_sorted_subset
  # Txx_sorted_subset_pretty = daily_records$Txx_sorted_subset_pretty
  # 
  # numrec_Tnx = daily_records$numrec_Tnx
  # Tnx_sorted_subset = daily_records$Tnx_sorted_subset
  # Tnx_sorted_subset_pretty = daily_records$Tnx_sorted_subset_pretty
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-temp_de.Rmd")
  
  # Add images 
  filename = "monatsbilanz_temp_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_temp(bulletin, valueBase = "abs", provisional = bulletin$provisional, mediaType = "image/png", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
  filename = "monatsbilanz_temp_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_temp(bulletin, valueBase = "anom", provisional = bulletin$provisional, mediaType = "image/png", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
  # Add images 
  filename = "monatsbilanz_temp_map_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "temp", filename = filename),
                        filename = filename,
                        caption = paste0("Monatsmitteltemperaturen in °C für den ",month[mon]," ",bulletin$year,"."))
  
  filename = "monatsbilanz_temp_map_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "temp", filename = filename),
                        filename = filename,
                        caption = paste0("Abweichungen der Monatsmitteltemperatur von der Norm 1991-2020 in °C für den ",month[mon]," ",bulletin$year,"."))
  
  # example table
#  regdata_table <- regdata_example_table(bulletin)
#  bulletin <- add_flextable(bulletin, flextable = regdata_table)
  
  bulletin
}

regdata_example_table <- function(bulletin) {
  regdata <- readRDS(system.file("example-data", "bulletin_monthly", "regdata-example.Rdata", package = "cat.bulletin"))
  df <- as.data.frame(regdata)
  df <- format(df)
  df$region <- rownames(regdata)
  df <- df[,c(4,1:3)]  # set column order
  
  table <- flextable::flextable(df) %>%
    flextable::set_header_labels(values =c("Region", "Mittelwert", "Minimum", "Maximum")) %>%
    flextable::add_header_row(
      values = c("", "Temperaturen"),
      colwidths = c(1,3)
    ) %>%
    flextable::bg(i = ~ as.numeric(TTanom_mean) < 0, j = "TTanom_mean", bg = "#EFEFEF", part = "body") %>%
    flextable::add_footer_lines("Example footer line") %>%
    flextable::set_caption("Regional temperature data") %>%
    flextable::set_table_properties(layout = "autofit")
  table
}

monatsbilanz_precip <- function(bulletin, regdiff) {

  mon = bulletin$month
  lang = "G"
  
  provisional = bulletin$provisional
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-precip_de.Rmd")
  
  # Add images 
  filename = "monatsbilanz_prec_map_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "prec", filename = filename),
                        filename = filename,
                        caption = paste0("Monatliche Niederschlagssumme in mm für den ",month[mon]," ",bulletin$year,"."))
  
  filename = "monatsbilanz_prec_map_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "prec", filename = filename),
                        filename = filename,
                        caption = paste0("Abweichung der monatlichen Niederschlagssumme von der Norm 1991-2020 für den ",month[mon]," ",bulletin$year,", dargestellt in Prozent der Norm."))
}

monatsbilanz_sun <- function(bulletin) {

  mon = bulletin$month
  lang = "G"

  provisional = bulletin$provisional
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_monatsbilanz-sun_de.Rmd")
  
  # Add images 
  filename = "monatsbilanz_sunshine_map_abs.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "sunshine", filename = filename),
                        filename = filename,
                        caption = paste0("Prozent der maximal möglichen Sonnenscheindauer für den ",month[mon]," ",bulletin$year,"."))
  
  filename = "monatsbilanz_sunshine_map_anom.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "sunshine", filename = filename),
                        filename = filename,
                        caption = paste0("Abweichung der monatlichen Sonnenscheindauer von der Norm 1991-2020 für den ",month[mon]," ",bulletin$year,", dargestellt in Prozent der Norm."))

}

temporal_evolution <- function(bulletin, swissmean) {
  
  mon = bulletin$month
  lang <- "G"
  
  month <- c(cat.lang::get.text("january",lang),
             cat.lang::get.text("february",lang),
             cat.lang::get.text("march",lang),
             cat.lang::get.text("april",lang),
             cat.lang::get.text("may",lang),
             cat.lang::get.text("june",lang),
             cat.lang::get.text("july",lang),
             cat.lang::get.text("august",lang),
             cat.lang::get.text("september",lang),
             cat.lang::get.text("october",lang),
             cat.lang::get.text("november",lang),
             cat.lang::get.text("december",lang))
  
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_temporal-evolution_de.Rmd")
  
}

monatsbulletin_daily_timeseries <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-daily_timeseries_de.Rmd")
  
  filename = "witterungsverlauf.png"
  bulletin <- add_image(bulletin = bulletin,
                        filepath = download_witterungsverlauf(bulletin, month=bulletin$month, year=bulletin$year, location="SMA", language="de", filename = filename),
                        filename = filename,
                        caption = "This is a caption.")
  
}

monatsbulletin_more_info <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_more-info_de.Rmd")
}

monatsbulletin_disclaimer <- function(bulletin) {
  bulletin <- add_Rmd(bulletin, filename = "bulletin-monthly_disclaimer_de.Rmd")
}

# further helping functions
add_article <- function(word) {
  # Check if the word starts with a vowel (a, e, i, o, u, y)
  if (grepl("^[aeéèiouyAEÉÈIOUY]", word)) {
    return(paste0("d'", tolower(word)))
  } else {
    return(paste0("de ", tolower(word)))
  }
}

ordinal_number <- function(number, gender, lang) {
  # Validate inputs
  if (!is.numeric(number)) stop("Number must be numeric.")
  if (!lang %in% c("G", "F", "I")) stop("Invalid language. Use G for German, F for French, I or Italian.")
  if (!gender %in% c("m", "f")) stop("Invalid gender. Use masculin or female.")
  
  # German case: add a period after the number
  if (lang == "G") {
    return(paste0(number, "."))
  }
  
  # French ordinal logic
  if (lang == "F") {
    if (gender == "m") {
      if (number == 1) return("1er")  # special case for 1st
      return(paste0(number, "e"))
    } else if (gender == "f") {
      if (number == 1) return("1re")  # special case for 1st (female)
      return(paste0(number, "e"))
    }
  }
  
  # Italian ordinal logic
  if (lang == "I") {
    suffix <- ifelse(gender == "m", "o", "a")
    if (number == 1) return(paste0(number, suffix))  # 1st is unique
    return(paste0(number, suffix))
  }
}

collapse_sentence <- function(strings) {
  n <- length(strings)
  
  # Handle different cases based on the number of strings
  if (n == 1) {
    return(strings)  # No need to collapse if there's only one string
  } else if (n == 2) {
    return(paste(strings, collapse = " und "))  # Two strings, collapse with " und "
  } else {
    # More than two strings, collapse with ", " and " und " for the last two
    return(paste(paste(strings[1:(n-1)], collapse = ", "), strings[n], sep = " und "))
  }
}

get_final_date <- function(year, month) {
  # Get the current year, month, and day
  current_date <- Sys.Date()
  current_year <- lubridate::year(current_date)
  current_month <- lubridate::month(current_date)
  current_day <- lubridate::day(current_date)
  
  # If the year and month are the current year and month
  if (year == current_year && month == current_month) {
    # Return current day minus 1
    last_date <- current_date - 1
  } else {
    # Get the last day of the specified month in the past
    last_date <- lubridate::ceiling_date(as.Date(paste(year, month, "01", sep = "-")), "month") - 1
  }
  Sys.setlocale("LC_TIME", "de_DE.UTF-8")
  last_date <- format(last_date, "%d. %B %Y")
  return(last_date)
}