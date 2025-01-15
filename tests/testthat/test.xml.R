test_that("Title to xml", {
  title = "# this is a title"
  language = "de"
  bulletin <- create_bulletin(languages = language) %>%
    add_title(title)
  
  xmlfile = bulletin_to_xml(bulletin)
  root <- xml2::read_xml(xmlfile)
  expect_true(xml2::xml_has_attr(root, languaged("title", language)))
})


test_that("Text to xml", {
  text <- "This is a text with *markdown*"
  language = "en"
  bulletin <- create_bulletin(languages = language) %>%
    add_text(text)
  
  xmlfile = bulletin_to_xml(bulletin)
  root <- xml2::read_xml(xmlfile)
  content_node <- xml2::xml_child(root, search = "content")
  text_node <- xml2::xml_child(content_node, search = "text")
  expect_true(xml2::xml_has_attr(text_node, languaged("html", language)))
  expect_equal(xml2::xml_attr(text_node, languaged("html", language)),
               "<p>This is a text with <em>markdown</em></p>")
})


test_that("Create xml from bulletin", {
  text <- c("# This is a title",
            "This is normal text."
  )
  bulletin <- create_bulletin() %>%
    add_text(text[1]) %>%
    add_text(text[2])
  
  filename <- bulletin_to_xml(bulletin)
  lines <- readr::read_lines(filename)
  
  expect_snapshot_file(filename)
})
