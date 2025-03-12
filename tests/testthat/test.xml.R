test_that("bulletin to xml", {
  text = "This is a text"

  bulletin <- create_minimal_bulletin() %>%
    set_active_language("de") %>%
    add_text(text)
  
  xmlfile = bulletin_to_xml(bulletin)
  root <- xml2::read_xml(xmlfile)
  expect_match(as.character(xml2::xml_find_first(root, "content") %>% xml2::xml_find_first("text")), text)
})


test_that("Markdown to xml", {
  text <- "This is a text with **markdown** and ä and &"
  bulletin <- create_minimal_bulletin() %>%
    add_text(text)
  
  xmlfile = bulletin_to_xml(bulletin)
  root <- xml2::read_xml(xmlfile)
  content_node <- xml2::xml_child(root, search = "content")
  text_node <- xml2::xml_child(content_node, search = "text")
  expect_true(xml2::xml_has_attr(text_node, languaged("html", bulletin$language)))
  expect_equal(xml2::xml_attr(text_node, languaged("html", bulletin$language)),
               "<p>This is a text with <strong>markdown</strong> and ä and &amp;</p>")
})

test_that("Image to xml", {
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png")
  caption  <- "this is a caption"

  bulletin <- create_minimal_bulletin() %>%
    add_image(filepath = filepath, caption = caption)

  xmlfile = bulletin_to_xml(bulletin)
  root <- xml2::read_xml(xmlfile)
  content_node <- xml2::xml_child(root, search = "content")
  image_node <- xml2::xml_child(content_node, search = "image")
  expect_true(xml2::xml_has_attr(image_node, languaged("fileName", bulletin$language)))
})
