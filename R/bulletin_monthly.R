#' Create the monthly bulletin
#' @param year Bulletin year
#' @param month Bulletin month
#' @param provisional boolean indicating if the provisional version of the bulletin shall be created
#' @param ... further general bulletin arguments forwarded to the create_bulletin function. Use them to set working directory etc. 
#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function(year = 2024, month = 8, provisional = FALSE, ...) {
  
  cat.func::assert.integer(year, "year", length = 1, minimum = 1900, maximum = 2100)
  cat.func::assert.integer(month, "month", length = 1, minimum = 1, maximum = 12)  
  assert_that(is.logical(provisional) && length(provisional) == 1)
  
  bulletin <- create_bulletin(bulletin_id = "bulletin-monthly",
                              workdir = ".",
                              languages = c("de", "fr", "it"),
                              bulletin_args = list(year = year,
                                                   month = month,
                                                   provisional = provisional,
                                                   yearmonth = paste0(year, sprintf("%02d", month))
                              ),
                              ...)
  
  bulletin <- set_monthly_bulletin_status(bulletin)
  
  swissmean <- calculate_swissmean_temp(bulletin)
  regdiff <- calculate_regional_differences(bulletin)
  
  metadata <- monatsbulletin_metadata(bulletin = bulletin,
                                      swissmean = swissmean,
                                      regdiff = regdiff)
  bulletin <- bulletin %>% 
    set_metadata(metadata) 
  
  for (language in bulletin[["languages"]]) {
      bulletin <- bulletin %>% set_active_language(language = language)
      bulletin$month_str <- cat.lang::get.text(paste("month", month, sep="."))
      bulletin$nextmonth_str <- cat.lang::get.text(paste("month", ifelse(month == 12, 1, month + 1), sep=".")) 
      bulletin <- bulletin %>%
        monatsbilanz_temp(swissmean = swissmean, regdiff = regdiff, language = language)
  }
  
  
  # bulletin <- bulletin %>%
  #   monatsbulletin_head(swissmean, regdiff) %>%
  #   monatsbulletin_disclaimer() %>%
  #   monatsbilanz_temp(swissmean = swissmean, regdiff = regdiff) %>%
  #   temporal_evolution(swissmean = swissmean, regdiff = regdiff) %>%
  #   monatsbilanz_precip(regdiff = regdiff) %>%
  #   monatsbilanz_sun(regdiff = regdiff) %>%
  #   monatsbulletin_daily_timeseries() %>%
  #   monatsbulletin_more_info()
  
  #bulletin_pdfxmlzip(bulletin)
  #bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
  #bulletin_to_xml(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.xml"))
  bulletin_to_webzip(bulletin)
}

monatsbulletin_metadata <- function(bulletin, swissmean, regdiff) {
  
  bulletin_path <- function(bulletin) {
    path <- paste0("reports-and-bulletins", "/",
                   bulletin$year, "/", 
                   "klimabulletin", "-", bulletin$month_str, "-", bulletin$year)
    tolower(path)
  }
  
  bulletin_title <- function(bulletin) {
    title = c(
      de = "Klimabulletin",
      fr = "Bulletin climatologique",
      it = "Bolletino del clima"
    )
    
    for (lang in names(title)) {
      title[lang] <- paste(title[lang], cat.lang::get.text(paste0("month.", bulletin$month), lang = cat.func::isolang2dwhlang(lang)))
      title[lang] <- paste(title[lang], bulletin$year)
    }
    
    title
  }
  
  metadata <- publication_metadata(
    path = bulletin_path(bulletin),
    title = bulletin_title(bulletin),
    lead = c(
      de = lore_ipsum("de"),
      it = lore_ipsum("it"),
      fr = lore_ipsum("fr")
    ),
    categories = c(
      de = "Klima",
      it = "Clima",
      fr = "Climat"
    ),
    teaser_image = monthlybulletin_teaser_image(yearmonth = bulletin$yearmonth),
    teaser_source = sapply(c("de", "it", "fr"), 
                           function(lang) 
                             monthlybulletin_teaser_text(yearmonth = bulletin$yearmonth, language = lang)
    ),
    keywords = c(),
    authors = c(
      de = "MeteoSchweiz",
      fr = "MeteoSuisse",
      it = "MeteoSvizzera"
    ),
    publishedAt = Sys.Date()
  )
  
  metadata
}

monatsbulletin_head <- function(bulletin, swissmean, regdiff) {
  
  log_info("bulletin head")
  
  title <- paste("# Klimabulletin", bulletin$month_str, bulletin$year)
  bulletin <- bulletin %>% add_title(title) 
  
  # Change the succession of these sentences based on a weight
  bulletin <- bulletin %>% add_Rmd(element_id = "leadtext")
  # bulletin <- bulletin %>% add_Rmd(element_id = "leadtext-temp")
  # bulletin <- bulletin %>% add_Rmd(element_id = "leadtext-precip")
  # bulletin <- bulletin %>% add_Rmd(element_id = "leadtext-sun")
  
  bulletin_prod_path <- get_config_value("bulletin_prod_path")
  yearmonth <- paste0(bulletin$year, sprintf("%02d", bulletin$month))
  
  # teaser text
  get_teaser_text <- function(bulletin_prod_path, yearmonth) {
    filepath <- file.path(bulletin_prod_path, yearmonth, paste0(yearmonth, "_teaser_text.txt"))
    if (assertthat::is.readable(filepath)) {
      lines <- readLines(filepath)
      if (length(lines) > 1)
        warning("teaser_text.txt contains more than one line. Using only the first.")
      lines[1]
    } else {
      log_debug("Did not find a teaser text for the current month. Using default...")
    }
  }
  
  teasertext <- get_teaser_text(bulletin_prod_path, yearmonth)
  
  #teaser image
  
  get_teaser_image <- function(bulletin_prod_path, yearmonth) {
    filepath = file.path(bulletin_prod_path, yearmonth, "teaser_image.jpg")
    if (assertthat::is.readable(filepath)) {
      filepath
    } else {
      log_debug("Did not find a teaser image for the current month. Using default...")
      system.file(package = "cat.bulletin", "example-data", "teaser-image.jpg")
    }
  } 
  
  bulletin <- bulletin %>% 
    add_image(filename = "teaser_image.jpg", 
              filepath = get_teaser_image(bulletin_prod_path, yearmonth),
              caption = teasertext)
  
  bulletin
}

monatsbilanz_temp <- function(bulletin, swissmean, regdiff, language) {
  
  log_info("monatsbilanz_temp")
  
  # bulletin <- bulletin %>% set_active_language(language = language)
  # if (bulletin$language != "de"){
  #   month <- sapply(bulletin$month_str,add_article)
  #   month <- as.character(month)
  # }
  
  # bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-temp-p1")

  # Add image for absolute temperatures
  image_id <- "monatsbilanz_temp_map_abs"
  filename_in <- paste0(image_id,".png")
  filename_out <- paste0(image_id,"_",language,".png")
  bulletin <- bulletin %>% 
    add_image(
      filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "temp", filename = filename_in),
      filename = filename_out,
      caption = glue::glue(cat.lang::get.text("bulletin_monthly_temp_map_abs")),
      id = image_id
    )
  
  # Add image for temperature anomalies
  image_id <- "monatsbilanz_temp_map_anom"
  filename_in <- paste0(image_id,".png")
  filename_out <- paste0(image_id,"_",language,".png")
  bulletin <- bulletin %>% 
    add_image(
      filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "temp", filename = filename_in),
      filename = filename_out,
      caption = glue::glue(cat.lang::get.text("bulletin_monthly_temp_map_anom")),
      id = image_id
    )
  
  
  # filename = "monatsbilanz_temp_map_anom.png"
  # bulletin <- bulletin %>% add_image(
  #   filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "temp", filename = filename),
  #   filename = filename,
  #   caption = paste0("Abweichungen der Monatsmitteltemperatur von der Referenzperiode 1991-2020 in \u00B0C für den ",bulletin$month_str," ",bulletin$year,". Abweichungen über der Referenz sind rot, Abweichungen unter der Referenz sind blau eingefärbt."))
  # 
  # bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-temp-p2")
  # 
  # bulletin <- bulletin %>% add_flextable(flextable = regdiff$subset_climtab_T)
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
  log_info("monatsbilanz_precip")
  
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p1")
  
  # Add images 
  filename = "monatsbilanz_prec_map_abs.png"
  bulletin <- bulletin %>% add_image(
    filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "prec", filename = filename),
    filename = filename,
    caption = paste0("Monatliche Niederschlagssumme in mm für den ",bulletin$month_str," ",bulletin$year,"."))
  
  filename = "monatsbilanz_prec_map_anom.png"
  bulletin <- bulletin %>% add_image(
    filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "prec", filename = filename),
    filename = filename,
    caption = paste0("Abweichung der monatlichen Niederschlagssumme von der Referenzperiode 1991-2020 für den ",bulletin$month_str," ",bulletin$year,", dargestellt in % der Referenz."))
  
  if (regdiff$high_prec_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p2-1")
  }
  if (regdiff$low_prec_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p2-2")
  }
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p3")
  
  bulletin <- bulletin %>% add_flextable(flextable = regdiff$subset_climtab_P)
  bulletin
}

monatsbilanz_sun <- function(bulletin, regdiff) {
  log_info("monatsbilanz_sun")
  
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p1")
  
  # Add images 
  filename = "monatsbilanz_sunshine_map_abs.png"
  bulletin <- bulletin %>% add_image(
    filepath = download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "sunshine", filename = filename),
    filename = filename,
    caption = paste0("Verhältnis der effektiven Sonnenscheindauer zur maximal möglichen Sonnenscheindauer für den ",bulletin$month_str," ",bulletin$year,"."))
  
  filename = "monatsbilanz_sunshine_map_anom.png"
  bulletin <- bulletin %>% add_image(
    filepath = download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "sunshine", filename = filename),
    filename = filename,
    caption = paste0("Abweichung der monatlichen Sonnenscheindauer von der Referenzperiode 1991-2020 für den ",bulletin$month_str," ",bulletin$year,", dargestellt in % der Referenz."))
  
  if (regdiff$high_sun_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p2-1")
  }
  if (regdiff$low_sun_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p2-2")
  }
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p3")
  
  bulletin <- bulletin %>% add_flextable(flextable = regdiff$subset_climtab_S)
  bulletin  
}

temporal_evolution <- function(bulletin, swissmean, regdiff) {
  
  log_info("temporal_evolution")
  
  bulletin <- bulletin %>% add_Rmd(element_id = "temporal-evolution-p1")
  
  if (regdiff$high_temp_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "temporal-evolution-p2-1")
  }
  if (regdiff$low_temp_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "temporal-evolution-p2-2")
  }
  
  bulletin <- bulletin %>% add_Rmd(element_id = "temporal-evolution-p3")
  
  bulletin
}

monatsbulletin_daily_timeseries <- function(bulletin) {
  
  log_info("monatsbulletin_daily_timeseries")
  
  bulletin <- bulletin %>% add_Rmd(element_id = "daily-timeseries")
  
  filename = "witterungsverlauf.png"
  bulletin <- bulletin %>% add_image(
    filepath = download_witterungsverlauf(bulletin, month=bulletin$month, year=bulletin$year, location="SMA", language=bulletin$language, filename = filename),
    filename = filename,
    caption = "This is a caption.")
  
  bulletin
}

monatsbulletin_more_info <- function(bulletin) {
  bulletin <- bulletin %>% add_Rmd(element_id = "more-info")
  bulletin
}

monatsbulletin_disclaimer <- function(bulletin) {
  bulletin <- bulletin %>% add_Rmd(element_id = "disclaimer")
  bulletin
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

ordinal_number <- function(number, gender, language) {
  lang = cat.func::isolang2dwhlang(language)
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

#' Get the publication date for the monthly bulletin formatted as string
#' @return a string of the date
#' @param year publication year
#' @param month publication month
#' @param language iso country id
get_final_date <- function(year, month, language) {
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
  locale <- paste0(language, "_CH.UTF-8")
  last_date <- withr::with_locale(
    new = c("LC_TIME" = locale),
    code = format(last_date, "%d. %B %Y")
  )
  return(last_date)
}

set_monthly_bulletin_status <- function(bulletin) {
  current_date <- Sys.Date()
  current_year <- as.integer(format(current_date, "%Y"))
  current_month <- as.integer(format(current_date, "%m"))
  
  # Check if predefined month is in the future
  if (bulletin$year > current_year || 
      (bulletin$year == current_year && bulletin$month > current_month)) {
    stop("Error: You cannot create a bulletin for a month in the future.\n
         Please make sure bulletin$year and bulletin$month either correspond to 
         the current or any past month.")
  }
  
  # If bulletin$year and $month == current --> provisional
  if (bulletin$year == current_year && bulletin$month == current_month) {
    bulletin$provisional <- TRUE
  } else {
    # otherwise --> definitive
    bulletin$provisional <- FALSE
  }
  
  return(bulletin)
}