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

test_that("has_element", {
  title <- "# This is a title"
  text <- "This is normal text."
  
  bulletin <- create_bulletin() %>%
    add_text(text) %>%
    add_title(title)
  
  text_element = bulletin$elements[[1]]
  title_element = bulletin$elements[[2]]
  
  expect_true(has_element(bulletin, type = "title"))
  expect_true(has_element(bulletin, type = "text"))
  expect_false(has_element(bulletin, type = "blabla"))
  expect_true(has_element(bulletin, id = title_element$id))
  expect_false(has_element(bulletin, id = "asdfasfd"))
  expect_error(has_element(bulletin, type = "asdf", id = "asdf"))
  expect_true(has_element(bulletin))
  
})

test_that("get_elements", {
  title <- "# This is a title"
  text <- "This is normal text."
  
  bulletin <- create_bulletin() %>%
    add_text(text) %>%
    add_title(title)
  
  text_element = bulletin$elements[[1]]
  title_element = bulletin$elements[[2]]
  
  expect_equal(get_elements(bulletin, type = "title"), list(title_element))
  expect_equal(get_elements(bulletin, type = "text"), list(text_element))
  expect_equal(get_elements(bulletin, type = "blabla"), list())
  expect_equal(get_elements(bulletin, id = title_element$id), list(title_element))
  expect_equal(get_elements(bulletin, id = "asdfasfd"), list())
  expect_error(get_elements(bulletin, type = "asdf", id = "asdf"))
  expect_equal(get_elements(bulletin), bulletin$elements)
  
})

test_that("frontmatter", {
  bulletin <- create_bulletin()
  filename <- tempfile()
  file_conn <- file(filename, open = "wb") # readr::write_lines only supports binary connections
  
  # write the R markdong front matter first
  write_markdown_frontmatter(bulletin = bulletin, file_conn = file_conn)
 close(file_conn)
  cat(paste(readLines(filename), collapse = "\n"))
})
