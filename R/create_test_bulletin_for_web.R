#' Create a test bulletin publication zip file for the website
#' @inheritParams bulletin_to_webzip
#' @inheritParams create_bulletin
#' @inheritParams publication_metadata
#' @param sections A subset of example sections to process. For faster processing, only a subset of elements can be processed.
#' @export
create_test_bulletin_for_web <- function(workdir = tempdir(),
                                         bulletin_dir = "bulletin",
                                         zipfilename = paste0(
                                           "webtest_",
                                           format(Sys.time(), format = "%Y%m%d%H%M"),
                                           ".zip"
                                         ),
                                         path = "test-bulletin",
                                         sections = c("text", "image", "markdown", "table", "shorties", "disclaimer", "link_list")) {
  
  sections <- match.arg(sections, several.ok = TRUE)
  
  log_info("Creating cat.bulletin test bulletin in workdir", workdir, style = "h1")
  
  bulletin <- create_bulletin(
    bulletin_id = "webtest",
    languages = c("de", "fr", "it"),
    bulletin_dir = bulletin_dir,
    workdir = workdir
  ) 
  
  log_info("Adding publication metadata")
  
  metadata <- publication_metadata(
    path = path,
    title = c(
      de = paste("Klimabulletin - Testpublikation", Sys.Date()),
      fr = "Bulletin climatologique - test",
      it = "Bolletino del clima - test"
    ),
    lead = c(
      de = lore_ipsum("de"),
      it = lore_ipsum("it"),
      fr = lore_ipsum("fr")
    ),
    teaser_image = system.file(package = "cat.bulletin", "example-data", "teaser-image.jpg"),
    teaser_source = c(
      de = "Foto: ",
      it = "Foto: ",
      fr = "Photo: "
    ),
    keywords = c()
  )
  
  bulletin <- bulletin %>% set_metadata(metadata)
  
  log_info("Adding elements")
  
  ## Disclaimer element
  if ("disclaimer" %in% sections) {
    text_id <- "disclaimer"
    bulletin <- bulletin %>%
      set_active_language(language = "de") %>%
      add_disclaimer(caption_text = "Das Bulletin wird jeweils 5 Tage vor Monatsende ein erstes Mal publiziert und ab dann täglich aufdatiert bis zum letzten Tag des Monats.",
                     body_Rmd_element_id = "element")
  }
  
  ## Text element
  
  if ("text" %in% sections) {
    text_id <- generate_element_id("text")
    bulletin <- bulletin %>%
      set_active_language(language = "de") %>%
      add_text("Das ist ein Text auf Deutsch mit ös und äs.", id = text_id) %>%
      set_active_language(language = "fr") %>%
      add_text("C'est un texte en français avec é et è.", id = text_id)%>%
      set_active_language(language = "it") %>%
      add_text("Questo è un testo in italiano con & et %.", id = text_id)
  }
  
  ## Image element
  
  if ("image" %in% sections) {
    image_id <- "my_first_image"
    filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png")
    bulletin <- bulletin %>%
      set_active_language(language = "de") %>%
      add_image(filepath = filepath, 
                filename = "image1_de.png", 
                caption = "Bildlegende",
                id = image_id)  %>%
      set_active_language(language = "fr") %>%
      add_image(filepath = filepath, 
                filename = "image1_fr.png", 
                caption = "Légende de l'image",
                id = image_id)  %>%
      set_active_language(language = "it") %>%
      add_image(filepath = filepath, 
                filename = "image1_it.png", 
                caption = "Legenda",
                id = image_id)
  }
  ## R-Markdown element
  
  if ("markdown" %in% sections) {
    element_id <- "my_first_Rmd"
    x <- 22 # needed by webtest_element_??.Rmd
    for (language in bulletin$languages)
      bulletin <- bulletin %>%
      set_active_language(language = language) %>%
      add_Rmd(element_id = "element",
              id = element_id)
  }
  
  ## Table element
  if ("table" %in% sections) {
    element_id <- "my_first_table"
    regdata <- readRDS(system.file("example-data", "bulletin_monthly", "regdata-example.Rdata", package = "cat.bulletin"))
    for (language in bulletin$languages)
      bulletin <- bulletin %>%
      set_active_language(language = language) %>%
      add_table(regdata,
                id = element_id,
                caption = paste(language, "caption"),
                colwidths = rep(2, ncol(regdata)),
                align = "lcccc"
      )
  }
  
  ## Shorties list element
  if ("shorties" %in% sections) {
    element_id = "my_shorties"
    for (language in bulletin$languages)
      bulletin <- bulletin %>%
        set_active_language(language = language) %>%
        add_shorties_list(group_id = "event",
                          title = cat.lang::get.text("bulletin_monthly_shorties_title"),
                          path = system.file(package = "cat.bulletin", "example-data", "shorties"),
                          id = element_id
        )
  }
  
  ## Link List element
  if ("link_list" %in% sections) {
    element_id = "myLinkList"
    for (language in bulletin$languages) {
      link_list <- link_list_element(title = "My Link List",
                                     id = element_id) %>%
        add_link(path = "/meteoswiss/.../", 
                 label = "MeteoSwiss Home",
                 url = "https://www-integ.meteoschweiz.ch/klima/klima-der-schweiz/klima-normwerte.html"
        ) %>%
        add_link(
          path = "/meteoswiss/homepage/service-and-publications/publications", 
          label = switch(language,
                         it = "Pubblicazioni",
                         de = "Publikationen",
                         fr = "Publications"
          ),
          url = switch(language,
                       de = "https://www-integ.meteoschweiz.ch/service-und-publikationen/publikationen.html",
                       fr = "https://www-integ.meteosuisse.ch/services-et-publications/publications.html",
                       it = "https://www-integ.meteosvizzera.ch/servizi-e-pubblicazioni/pubblicazioni.html"
          )
        )
      
      bulletin <- bulletin %>%
        set_active_language(language = language) %>%
        add_link_list(link_list)
    }
  }
  
  log_info("Finished adding elements", style = "success")
  
  ## Generate pdfs in all languages and the xml and zip everything up
  
  #language = "de"
  #filename <- bulletin_to_pdf(bulletin, filename = file.path(bulletin$files_path, languaged_filename(bulletin$bulletin_id, language, "pdf")), language = language)
  
  #bulletin_to_xml(bulletin, filename = file.path(bulletin$bulletin_path, "publication.xml"))
  
  bulletin_to_webzip(bulletin = bulletin, zipfilename = zipfilename)
  
  invisible(bulletin)
}
