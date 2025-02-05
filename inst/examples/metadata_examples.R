# Create publication metadata
my_metadata <- publication_metadata(
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