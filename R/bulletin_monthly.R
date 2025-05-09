#' deprecated short for creating the monthly bulletin webzip 
#' @details
#' Use \code{bulletin_monthly} and one of the rendering functions instead.
#' @inheritParams bulletin_monthly
#' @export
create_bulletin_monthly <- function(year = 2024, 
                                    month = 8,
                                    workdir = ".",
                                    ...) {
  
  bulletin <- bulletin_monthly(year = year, month = month, workdir = workdir, ...)
  
  zipfilename = paste0("climate-bulletin-", bulletin$year, "-", bulletin$month,
                       "-", format(Sys.time(), format = "%Y%m%d%H%M"),
                       ".zip")
  
  bulletin_to_webzip(bulletin, zipfilename = zipfilename)
  
}


#' Create the monthly bulletin
#' @param year Bulletin year
#' @param month Bulletin month
#' @param provisional boolean indicating if the provisional version of the bulletin shall be created. If missing, this will determined using 
#' \code{get_bulletin_monthly_provisional}.
#' @param ... further general bulletin arguments forwarded to the create_bulletin function. Use them to set working directory etc. 
#' @inheritParams create_bulletin
#' @importFrom magrittr %>%
#' @export
#' @examples 
#' bulletin <- bulletin_monthly(year = 2024, month = 8)
bulletin_monthly <- function(year = 2024, 
                             month = 8, 
                             provisional,
                             workdir = ".",
                             ...) {
  
  cat.func::assert.integer(year, "year", length = 1, minimum = 1900, maximum = 2100)
  cat.func::assert.integer(month, "month", length = 1, minimum = 1, maximum = 12) 
  if (missing(provisional))
    provisional <- get_bulletin_monthly_provisional(year = year, month = month)
  assert_that(is.logical(provisional) && length(provisional) == 1)
  
  log_info("Creating bulletin monthly for year =", year, "and month = ", month,".", style = "h1")
  log_debug("Provisional:", provisional, "; workdir:", workdir)
  
  bulletin <- create_bulletin(bulletin_id = "bulletin-monthly",
                              bulletin_dir = "climate-bulletin-monthly",
                              workdir = workdir,
                              languages = c("de", "fr", "it"),
                              bulletin_args = list(year = year,
                                                   month = month,
                                                   provisional = provisional,
                                                   yearmonth = paste0(year, sprintf("%02d", month))
                              ),
                              ...)
  
  swissmean <- calculate_swissmean_temp(bulletin)
  regdiff <- calculate_regional_differences(bulletin)
  
  for (language in bulletin[["languages"]]) {
    log_info("Adding bulletin elements for language", language, style = "h2")
    
    bulletin <- bulletin %>% set_active_language(language = language)
    
    # add lead (add default if no Rmd element exists)
    bulletin <- add_Rmd(bulletin = bulletin, element_id = "leadtext", appear = c())
    
    # add sections
    bulletin <- bulletin %>%
      add_bulletin_monthly_disclaimer(language = language) %>% 
      monatsbilanz_temp(swissmean = swissmean, regdiff = regdiff, language = language) %>%
      temporal_evolution(swissmean = swissmean, regdiff = regdiff, language = language) %>%
      monatsbilanz_precip(regdiff = regdiff, language = language) %>%
      monatsbilanz_sun(regdiff = regdiff, language = language) %>%
      monthly_events(language = language) %>%
      monthly_vegetation(language = language) %>%
      monatsbulletin_daily_timeseries(language = language)
  }
  
  log_info("Creating publication metadata", style = "h2")
  
  metadata <- monatsbulletin_metadata(bulletin = bulletin,
                                      lead_element_id = "leadtext",
                                      swissmean = swissmean,
                                      regdiff = regdiff)
  
  bulletin <- bulletin %>% 
    set_metadata(metadata) 
  
  log_info("Finished creating bulletin monthly for year =", year, "and month = ", month, ".", style = "success")
  
  invisible(bulletin)
}

#' Create the publication_metadata for the monthly bulletin.
#' @inheritParams create_bulletin_monthly
#' @inheritParams bulletin_to_webzip
#' @param lead_element_id the id of the (hidden) bulletin element that contains the lead text. Can be either of type Rmd or text. 
#' @param swissmean output of \code{calculate_swissmean_temp}
#' @param regdiff output of \code{calculate_regional_differences}
monatsbulletin_metadata <- function(bulletin, lead_element_id, swissmean, regdiff) {
  
  assert_that(is.character(lead_element_id), length(lead_element_id) == 1)
  
  bulletin_lead <- function(bulletin, lead_element_id, language) {
    #return(lore_ipsum(language = language))
    assert_that(has_element(bulletin = bulletin, language = language, id = lead_element_id))
    lead_element <- get_elements(bulletin = bulletin, language = language, id = lead_element_id)[[1]]
    bulletin = set_active_language(bulletin, language = language)
    lead <- switch(lead_element$type,
                   text = lead_element$text,
                   Rmd = Rmd_to_text(element = lead_element),
                   stop("lead element type not supported")
    )
    return(lead)
  }
  
  bulletin_edition <- function(language, provisional) {
    id <- ifelse(provisional, "edition_provisional", "edition_definitive")
    cat.lang::get.text(id, lang = cat.func::isolang2dwhlang(language))
  }
  
  bulletin_path <- function(bulletin) {
    path <- paste0("klimabulletin", "-", bulletin$year, "-", bulletin$month)
    tolower(path)
  }
  
  bulletin_title <- function(bulletin) {
    title = c(
      de = "Klimabulletin",
      fr = "Bulletin climatologique",
      it = "Bolletino del clima"
    )
    
    for (lang in names(title)) {
      if (lang == "fr") {
        title[lang] <- paste(title[lang], tolower(month_str(bulletin$month, language = lang)))  
      } else {
        title[lang] <- paste(title[lang], month_str(bulletin$month, language = lang))  
      }
      title[lang] <- paste(title[lang], bulletin$year)
    }
    
    title
  }
  
  metadata <- publication_metadata(
    path = bulletin_path(bulletin),
    title = bulletin_title(bulletin),
    lead = sapply(bulletin$languages, 
                  function(lang) bulletin_lead(bulletin = bulletin, 
                                               lead_element_id = lead_element_id,
                                               language = lang)
    ),
    edition = sapply(bulletin$languages, bulletin_edition, provisional = bulletin$provisional),
    teaser_image = monthlybulletin_teaser_image(yearmonth = bulletin$yearmonth),
    teaser_source = sapply(bulletin$languages, 
                           function(lang) 
                             monthlybulletin_teaser_text(yearmonth = bulletin$yearmonth, language = lang)
    ),
    keywords = c(),
    publishedAt = Sys.Date()
  )
  
  metadata
}

add_bulletin_monthly_disclaimer <- function(bulletin, language) {
  bulletin <- bulletin %>%
    add_disclaimer(caption_text = "Das Bulletin wird jeweils 5 Tage vor Monatsende ein erstes Mal publiziert und ab dann täglich aufdatiert bis zum letzten Tag des Monats.",
                   body_Rmd_element_id = "disclaimer",
                   id = "disclaimer")
}

monatsbilanz_temp <- function(bulletin, swissmean, regdiff, language) {
  
  log_info("Processing monatsbilanz_temp")
  
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-temp-p1")
  
  ## Add joined image with monthly temperature maps (abs/anom)
  image_id <- "monatsbilanz_temp_map"
  image_filename <- paste0(image_id,".png")
  
  abs_filepath <- download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "temp", out_path = bulletin$cache_path, filename = "monatsbilanz_temp_map_abs.png")
  anom_filepath <- download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "temp", out_path = bulletin$cache_path, filename = "monatsbilanz_temp_map_anom.png")
  
  joined_and_cropped_filepath <- join_and_crop_monthly_maps(abs_filepath, anom_filepath)
  
  bulletin <- bulletin %>% 
    add_image(
      filepath = joined_and_cropped_filepath,
      caption = glue::glue(cat.lang::get.text("bulletin_monthly_temp_map")),
      alt = cat.lang::get.text("bulletin_monthly_temp_map_alt"),
      id = image_id
    )
  
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-temp-p2")
  
  ## Add table
  temp_table <- regdiff$subset_climtab_T
  colnames(temp_table) <- c(cat.lang::get.text("climtab_stat"),
                            cat.lang::get.text("climtab_altitude"),
                            cat.lang::get.text("climtab_temp_mean"),
                            cat.lang::get.text("climtab_temp_ref"),
                            cat.lang::get.text("climtab_temp_dev")
  )
  if (get_log_level() >= 2) print(temp_table)
  bulletin <- bulletin %>%
    add_table(temp_table, id = "monatsbilanz_temp_table",
              caption = glue::glue(cat.lang::get.text("bulletin_monthly_temp_table")),
              colwidths = c(5,rep(2, ncol(temp_table) - 1)),
              align = "lcccc"
    )
  
  bulletin
}

join_and_crop_monthly_maps <- function(abs_filepath, anom_filepath) {
  
  joined_filepath <- join_images(
    image_filepaths = c(abs_filepath, anom_filepath),
    outpath = tempfile(fileext = ".png")
  )
  
  cropped_filepath <- crop_image(joined_filepath,
                                 outpath = tempfile(fileext = ".png"),
                                 side = "top",
                                 margin = 23)
  
  cropped_filepath
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

monatsbilanz_precip <- function(bulletin, regdiff, language) {
  log_info("monatsbilanz_precip")
  
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p1")
  
  ## Add joined image with monthly precipitation maps (abs/anom)
  image_id <- "monatsbilanz_prec_map"
  image_filename <- paste0(image_id,".png")
  
  abs_filepath <- download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "prec", out_path = bulletin$cache_path, filename = "monatsbilanz_prec_map_abs.png")
  anom_filepath <- download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "prec", out_path = bulletin$cache_path, filename = "monatsbilanz_prec_map_anom.png")
  
  joined_and_cropped_filepath <- join_and_crop_monthly_maps(abs_filepath, anom_filepath)
  
  bulletin <- bulletin %>% 
    add_image(
      filepath = joined_and_cropped_filepath,
      caption = glue::glue(cat.lang::get.text("bulletin_monthly_prec_map")),
      alt = cat.lang::get.text("bulletin_monthly_prec_map_alt"),
      id = image_id
    )
  
  if (regdiff$high_prec_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p2-1")
  }
  if (regdiff$low_prec_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p2-2")
  }
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-precip-p3")
  
  ## Add table
  prec_table <- regdiff$subset_climtab_P
  colnames(prec_table) <- c(cat.lang::get.text("climtab_stat"),
                            cat.lang::get.text("climtab_altitude"),
                            cat.lang::get.text("climtab_prec_mean"),
                            cat.lang::get.text("climtab_prec_ref"),
                            cat.lang::get.text("climtab_prec_dev")
  )
  if (get_log_level() >= 2) print(prec_table)
  bulletin <- bulletin %>%
    add_table(prec_table, id = "monatsbilanz_prec_table",
              caption = glue::glue(cat.lang::get.text("bulletin_monthly_prec_table")),
              colwidths = c(5,rep(2, ncol(prec_table) - 1)),
              align = "lcccc")
  
  bulletin
}

monatsbilanz_sun <- function(bulletin, regdiff, language) {
  log_info("monatsbilanz_sun")
  
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p1")
  
  if (!bulletin$provisional) {
    ## Add joined image with monthly sunshine maps (abs/anom)
    image_id <- "monatsbilanz_sunshine_map"
    image_filename <- paste0(image_id,".png")
    
    abs_filepath <- download_monatsbilanz_maps(bulletin, valueBase = "abs", provisional = bulletin$provisional, parameter = "sunshine", out_path = bulletin$cache_path, filename = "monatsbilanz_sunshine_map_abs.png")
    anom_filepath <- download_monatsbilanz_maps(bulletin, valueBase = "anom9120", provisional = bulletin$provisional, parameter = "sunshine", out_path = bulletin$cache_path, filename = "monatsbilanz_sunshine_map_anom.png")
    
    joined_and_cropped_filepath <- join_and_crop_monthly_maps(abs_filepath, anom_filepath)
    
    bulletin <- bulletin %>% 
      add_image(
        filepath = joined_and_cropped_filepath,
        caption = glue::glue(cat.lang::get.text("bulletin_monthly_sunshine_map")),
        alt = cat.lang::get.text("bulletin_monthly_sunshine_map_alt"),
        id = image_id
      )
  }
  
  if (regdiff$high_sun_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p2-1")
  }
  if (regdiff$low_sun_rec_avail) {
    bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p2-2")
  }
  bulletin <- bulletin %>% add_Rmd(element_id = "monatsbilanz-sun-p3")
  
  ## Add table
  sun_table <- regdiff$subset_climtab_S
  colnames(sun_table) <- c(cat.lang::get.text("climtab_stat"),
                           cat.lang::get.text("climtab_altitude"),
                           cat.lang::get.text("climtab_sun_mean"),
                           cat.lang::get.text("climtab_sun_ref"),
                           cat.lang::get.text("climtab_sun_dev")
  )
  if (get_log_level() >= 2) print(sun_table)
  bulletin <- bulletin %>%
    add_table(sun_table, id = "monatsbilanz_sun_table",
              caption = glue::glue(cat.lang::get.text("bulletin_monthly_sun_table")),
              colwidths = c(5,rep(2, ncol(sun_table) - 1)),
              align = "lcccc")
  
  bulletin  
}

temporal_evolution <- function(bulletin, swissmean, regdiff, language) {
  
  log_info("temporal_evolution")
  
  # Add LOESS figure
  image_id <- "temperature_evolution_loess"
  
  image_filepath <- download_temporal_evolution(bulletin, valueBase = "climanom", provisional = bulletin$provisional, trend = "loess30nostats", mediaType = "image/png")
  
  cropped_filepath <- crop_image(image_filepath,
                                 outpath = tempfile(fileext = ".png"),
                                 side = "bottom",
                                 margin = 85)
  
  bulletin <- bulletin %>% 
    add_image(
      filepath = cropped_filepath,
      caption = glue::glue(cat.lang::get.text("bulletin_monthly_evolution")),
      alt = glue::glue(cat.lang::get.text("bulletin_monthly_evolution_alt")),
      id = image_id
    )
  
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

monthly_events <- function(bulletin, language) {
  
  log_info("monthly_events")
  
  bulletin_prod_path <- get_config_value("bulletin_prod_path")
  if (!see_if(is.readable(bulletin_prod_path))) {
    log_info("bulletin_prod_path not readable -> omitting monthly_events section", style = "warning")
    return(bulletin)
  }
  
  element_id = "event_list"
  
  bulletin <- bulletin %>% 
    add_shorties_list(group_id = "event",
                      title = cat.lang::get.text("bulletin_monthly_events_title"),
                      path =  bulletin_prod_path,
                      subdir = bulletin$yearmonth,
                      id = element_id)
  
  bulletin
}

monthly_vegetation <- function(bulletin, language) {
  
  log_info("monthly_vegetation")
  
  bulletin_prod_path <- get_config_value("bulletin_prod_path")
  if (!see_if(is.readable(bulletin_prod_path))) {
    log_info("bulletin_prod_path not readable -> omitting monthly_vegetation section", style = "warning")
    return(bulletin)
  }
  
  element_id = "vegetation_list"
  
  bulletin <- bulletin %>% 
    add_shorties_list(group_id = "vegetation",
                      title = cat.lang::get.text("bulletin_monthly_vegetation_title"),
                      path =  bulletin_prod_path,
                      subdir = bulletin$yearmonth,
                      id = element_id)
  
  bulletin
}


monatsbulletin_daily_timeseries <- function(bulletin, language) {
  
  log_info("monatsbulletin_daily_timeseries")
  
  # witterungsverlauf_station <- c(de = "SMA", fr = "GVE", it = "LUG")
  witterungsverlauf_station <- list(
    de = c("SMA", "ENG"),
    fr = c("GVE", "CHD"),
    it = c("LUG", "SAM")
  )
  
  wv_statname1 <- mchdwh::station_info(nat_abbr=witterungsverlauf_station[[language]][1])$station_name
  wv_statname2 <- mchdwh::station_info(nat_abbr=witterungsverlauf_station[[language]][2])$station_name
  
  bulletin <- bulletin %>% add_Rmd(element_id = "daily-timeseries")
  
  # Add image for daily weather conditions
  image_id <- "witterungsverlauf"

  image_filepath1 = download_witterungsverlauf(bulletin, month=bulletin$month, year=bulletin$year, location=as.character(witterungsverlauf_station[[language]][1]), language=language)
  image_filepath2 = download_witterungsverlauf(bulletin, month=bulletin$month, year=bulletin$year, location=as.character(witterungsverlauf_station[[language]][2]), language=language)
  
  joined_filepath <- join_images(
    image_filepaths = c(image_filepath1, image_filepath2),
    outpath = tempfile(fileext = ".png")
  )
  
  bulletin <- bulletin %>% 
    add_image(
      filepath = joined_filepath,
      caption = glue::glue(cat.lang::get.text("daily_timeseries")),
      alt = "test",
      id = image_id
    )
  
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
add_article <- function(word, lang = "fr", to_lower = TRUE) {
  # Check if the word starts with a vowel (h, a, e, i, o, u, y)
  if (grepl("^[aeéèiouyAEÉÈIOUY]", word)) {
    article <- "d'"
  } else {
    article <- ifelse(lang == "fr", "de ", "di ")
  }
  
  transformed_word <- if (to_lower) tolower(word) else word
  return(paste0(article, transformed_word))
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

collapse_sentence <- function(strings, language = "de") {
  n <- length(strings)
  
  # Handle different cases based on the number of strings
  if (n == 1) {
    return(strings)  # No need to collapse if there's only one string
  } else if (n == 2) {
    if (language == "de") {
      return(paste(strings, collapse = " und "))  # Two strings, collapse with " und "
    }
    if (language == "fr") {
      return(paste(strings, collapse = " et "))  # Two strings, collapse with " et "
    }
    if (language == "it") {
      return(paste(strings, collapse = " e "))  # Two strings, collapse with " e "
    }
    
  } else {
    # More than two strings, collapse with ", " and " und " for the last two
    if (language == "de") {
      return(paste(paste(strings[1:(n-1)], collapse = ", "), strings[n], sep = " und "))
    }
    if (language == "fr") {
      return(paste(paste(strings[1:(n-1)], collapse = ", "), strings[n], sep = " et "))
    }
    if (language == "it") {
      return(paste(paste(strings[1:(n-1)], collapse = ", "), strings[n], sep = " e "))
    }
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
  locale <- get_locale(language)
  last_date <- withr::with_locale(
    new = c("LC_TIME" = locale),
    code = format(last_date, "%d. %B %Y")
  )
  return(last_date)
}

#' Get default provisional value for \code{create_bulletin_monthly}
#' @inheritParams create_bulletin_monthly
#' @return  boolean value to use for the provisional parameter in \code{create_bulletin_monthly}
#' @export
get_bulletin_monthly_provisional <- function(year, month) {
  current_date <- Sys.Date()
  current_year <- as.integer(format(current_date, "%Y"))
  current_month <- as.integer(format(current_date, "%m"))
  
  # Check if predefined month is in the future
  if (year > current_year || 
      (year == current_year && month > current_month)) {
    stop("Error: You cannot create a bulletin for a month in the future.\n
         Please make sure bulletin$year and bulletin$month either correspond to 
         the current or any past month.")
  }
  
  # If year and month == current --> provisional
  if (year == current_year && month == current_month) {
    provisional <- TRUE
  } else {
    # otherwise --> definitive
    provisional <- FALSE
  }
  
  return(provisional)
}

month_str <- function(month, language) {
  cat.func::assert.integer(month, length = 1, minimum = 1, maximum = 12, name = "month")
  if (missing(language)) language = NULL else language = cat.func::isolang2dwhlang(language)
  cat.lang::get.text(paste0("month.", month), lang = language)
}

nextmonth_str <- function(month, language) {
  cat.func::assert.integer(month, length = 1, minimum = 1, maximum = 12, name = "month")
  month_str(ifelse(month == 12, 1, month + 1), language = language)
}

translate_regions <- function(text, lang = "fr") {
  dict_fr <- list(
    "Alpennordhang" = "sur le versant nord des Alpes",
    "Nord- und Mittelbünden" = "sur le nord et le centre des Grisons",
    "Jura" = "dans le Jura",
    "Alpensüdseite" = "au Sud des Alpes",
    "Mittelland" = "sur le Plateau",
    "Wallis" = "en Valais",
    "Engadin" = "en Engadine"
  )
  
  dict_it <- list(
    "Alpennordhang" = "lungo il versante nordalpino",
    "Nord- und Mittelbünden" = "nel nord e nel centro dei Grigioni",
    "Jura" = "nel Giura",
    "Alpensüdseite" = "al Sud delle Alpi",
    "Mittelland" = "nell'Altopiano",
    "Wallis" = "nel Vallese",
    "Engadin" = "nell'Engadina"
  )
  
  dict <- switch(lang,
                 "fr" = dict_fr,
                 "it" = dict_it,
                 stop("Ungültige Sprache. Verwenden Sie 'fr' oder 'it'."))
  
  pattern <- paste(names(dict), collapse = "|")  # Erzeuge Regex-Muster für alle Regionen
  matches <- unlist(regmatches(text, gregexpr(pattern, text, perl = TRUE)))  # Finde passende Regionen
  
  translated_parts <- unname(sapply(matches, function(x) dict[[x]]))
  
  conjunction <- ifelse(lang == "fr", "et", "e")
  
  if (length(translated_parts) > 1) {
    paste(paste(translated_parts[-length(translated_parts)], collapse = ", "), conjunction, translated_parts[length(translated_parts)])
  } else {
    translated_parts
  }
}

translate_stations <- function(input_string, lang, use_art = FALSE) {
  conjunction <- ifelse(lang == "fr", "et", "e")
  
  stations <- unlist(strsplit(input_string, " und "))
  
  if (use_art) {
    translated_stations <- sapply(stations, add_article, lang = lang, to_lower = FALSE)
  } else {
    translated_stations <- stations
  }
  
  result <- paste(translated_stations, collapse = paste0(" ", conjunction, " "))
  
  return(result)
}

num_to_word <- function(num, lang) {
  words <- list(
    de = c("einer", "zwei", "drei", "vier", "fünf", "sechs", 
           "sieben", "acht", "neun", "zehn", "elf", "zwölf"),
    fr = c("une", "deux", "trois", "quatre", "cinq", "six", 
           "sept", "huit", "neuf", "dix", "onze", "douze"),
    it = c("una", "due", "tre", "quattro", "cinque", "sei", 
           "sette", "otto", "nove", "dieci", "undici", "dodici")
  )
  
  if (!lang %in% names(words)) {
    stop("Wrong language. Use 'de', 'fr' or 'it'.")
  }
  
  if (num > 12) {
    return(num)
  } else {
    return(words[[lang]][num])
  }
}

translate_record_text <- function(text, language = c("fr", "it")) {
  language <- match.arg(language)
  
  # Define translations
  translations <- list(
    fr = list(
      and_word = "et",
      record_phrase1 = "record précédent ",
      record_phrase2 = "record "
    ),
    it = list(
      and_word = "e",
      record_phrase1 = "record precedente",
      record_phrase2 = "record"
    )
  )
  
  tr <- translations[[language]]
  
  # Replace " und " with translated "and"
  text <- gsub("\\bund\\b", tr$and_word, text)
  
  # Replace "bisheriger Rekord" or just "Rekord"
  text <- gsub("\\bbisheriger Rekord\\b", tr$record_phrase1, text)
  text <- gsub("\\bRekord\\b", tr$record_phrase2, text)
  
  if (language == "it") {
    # Add "con" between place name and value+unit (generalized)
    text <- gsub(
      "(\\b[A-ZÄÖÜa-zäöüßéèàùîç\\-]+)\\s+(\\d+(?:\\.\\d+)?\\s*[^\\s,()]+)",
      "\\1 con \\2",
      text,
      perl = TRUE
    )
    
    # Replace "h" with "ore" (e.g., "4.2 h" → "4.2 ore") everywhere
    text <- gsub("\\b(\\d+(?:\\.\\d+)?)\\s*h\\b", "\\1 ore", text)
  }
  
  return(text)
}