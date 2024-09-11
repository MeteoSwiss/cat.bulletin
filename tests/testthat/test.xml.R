test_that("Text to xml", {
  text <- "# This is a title"
  element <- text_element(text) 
  xml <- xml2::xml_new_root("Test")
  xml <- text_to_xml(xml = xml, element = element)
  #expect_snapshot_output(xml)
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
