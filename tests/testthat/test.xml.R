test_that("Text to xml", {
  text <- "# This is a title"
  element <- text_element(text) 
  xml <- xml2::xml_new_root("Test")
  xml <- text_to_xml(xml = xml, element = element)
  #expect_snapshot_output(xml)
})

