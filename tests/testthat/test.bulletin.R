test_that("Create bulletin with text elements", {
  bulletin <- create_bulletin() %>%
    add_text("# This is a title") %>%
    add_text("This is normal text.")
  expect_named(bulletin, "elements")
  expect_equal(length(bulletin$elements), 2)
})


test_that("Create markdown file from bulletin", {
  text <- c("# This is a title",
               "This is normal text."
               )
  bulletin <- create_bulletin() %>%
    add_text(text[1]) %>%
    add_text(text[2])
  
  filename <- bulletin_to_markdown(bulletin)
  md <- readr::read_lines(filename)
  expect_equal(md, text)
})

test_that("Create pdf from bulletin", {
  text <- c("# This is a title",
            "This is normal text."
  )
  bulletin <- create_bulletin() %>%
    add_text(text[1]) %>%
    add_text(text[2])
  
  filename <- bulletin_to_pdf(bulletin)
  expect_snapshot_file(filename)
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
