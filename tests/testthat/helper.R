create_minimal_bulletin <- function() {
  title = "this is a title"
  metadata <- publication_metadata(
    path = "webtest",
    title = c(
      de = "Klimabulletin - Testpublikation"
    ),
    lead = c(
      de = lore_ipsum("de")
    ),
    categories = c(
      de = "Klima"
    ),
    teaser_image = system.file(package = "cat.bulletin", "example-data", "teaser-image.jpg"),
    teaser_source = c(
      de = "Foto: "
    ),
    keywords = c(),
    authors = c(
      de = "MeteoSchweiz"
    ),
    publishedAt = Sys.Date()
  )
  bulletin <- create_bulletin(languages = c("de"),
                              metadata = metadata)
  bulletin
}