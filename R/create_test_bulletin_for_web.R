#' Create a test bulletin publication zip file for the website
#' @export
create_test_bulletin_for_web <- function(workdir = tempdir(),
                                         zipfilename = "climate-bulletin-webtest.zip") {
  bulletin <- create_bulletin(
    bulletin_id = "webtest",
    language = c("de", "fr", "it"),
    bulletin_dir = "webtest",
    workdir = workdir
  ) 
  
  metadata <- publication_metadata(
    path = "webtest",
    title = c(
      de = "Klimabulletin - Testpublikation",
      fr = "Bulletin climatologique - test",
      it = "Bolletino del clima - test"
    ),
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
    teaser_image = teaser_image(
      filepath = system.file(package = "cat.bulletin", "example-data", "teaser-image.jpg")
    ),
    teaser_source = c(
      de = "Foto: ",
      it = "Foto: ",
      fr = "Photo: "
    ),
    keywords = c(),
    authors = c(
      de = "MeteoSchweiz",
      fr = "MeteoSuisse",
      it = "MeteoSvizzera"
    ),
    publishedAt = Sys.Date()
  )
  
  bulletin <- bulletin %>% set_metadata(metadata)
  
  ## Text element
  
  text_id <- generate_element_id("text")
  bulletin <- bulletin %>%
    set_active_language(language = "de") %>%
    add_text("Das ist ein Text auf Deutsch mit ös und äs.", id = text_id) %>%
    set_active_language(language = "fr") %>%
    add_text("C'est un texte en français avec é et è.", id = text_id)%>%
    set_active_language(language = "it") %>%
    add_text("Questo è un testo in italiano con & et %.", id = text_id)
  
  ## Image element
  
  image_id <- "my_first_image"
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png")
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
  
  ## R-Markdown element
  
  element_id <- "my_first_Rmd"
  x <- 22 # needed by webtest_element_??.Rmd
  for (language in bulletin$languages)
    bulletin <- bulletin %>%
    set_active_language(language = language) %>%
    add_Rmd(element_id = "element",
            id = element_id)
  
  bulletin_to_webzip(bulletin = bulletin, zipfilename = zipfilename)
  
  bulletin
}