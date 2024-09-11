test_that("Text to markdown", {
  text <- "# This is a title"
  text_element <- text_element(text) 
  md <- text_to_markdown(text_element)
  expect_equal(md, text)
})

