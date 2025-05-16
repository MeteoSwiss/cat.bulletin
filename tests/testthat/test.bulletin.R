
test_that("Create bulletin contains element slots for all languages", {
  bulletin <- create_bulletin() 
  expect_contains(names(bulletin), languaged("elements", bulletin$languages))
})
  
test_that("Create bulletin with text elements", {
  language = "de"
  bulletin <- create_bulletin(languages = language) %>%
    add_text("# This is a title") %>%
    add_text("This is normal text.")
  expect_length(bulletin[[languaged_elements(language)]], 2)
})


test_that("Create markdown file from bulletin", {
  text <- c("# This is a title",
            "This is normal text."
  )
  bulletin <- create_bulletin() %>%
    add_text(text[1]) %>%
    add_text(text[2])
  
  filename <- bulletin_to_markdown(bulletin, filename = "markdown_test.md")
  md <- readr::read_lines(filename)
  expect_snapshot(md)
})

test_that("Create pdf from bulletin", {
  text <- c("## This is a level two title",
            "This is normal text."
  )
  bulletin <- create_bulletin() %>%
    add_text(text[1]) %>%
    add_text(text[2])
  
  filename <- bulletin_to_pdf(bulletin, language = "de")
  expect_snapshot_file(filename, name = "test_bulletin.pdf")
})


test_that("Create pdf from bulletin with metadata", {
  text <- c("## This is a level two title",
            "This is normal text."
  )
  bulletin <- create_bulletin(
    languages = "en",
    metadata = publication_metadata(
     title = c(en = "This is the bulletin title"),
     lead = c(en = "Im Leadtext Reihenfolge der zu nennenden Parameter über die Ränge entscheiden. Super wären Sätze im Sinne von DER AUGUST 2024 WAR GEPRÄGT VON HOHEN TEMPERATUREN UND WENIG NIEDERSCHLAG."),
     teaser_image = monthlybulletin_teaser_image(yearmonth = format(Sys.Date(), "%Y%m")),
     teaser_source = c(
       en = monthlybulletin_teaser_text(yearmonth = format(Sys.Date(), "%Y%m"), language = "de")
     )
    )
  ) %>%
    add_text(text[1]) %>%
    add_text(text[2])
  
  filename <- bulletin_to_pdf(bulletin, language = "en")
  expect_snapshot_file(filename, name = "test_bulletin_metadata.pdf")
})


test_that("has_element", {
  title <- "# This is a title"
  text <- "This is normal text."
  
  language = "de"
  
  bulletin <- create_bulletin(languages = language) %>%
    add_text(text) %>%
    add_text(title)
  
  text_element = bulletin[[languaged_elements(language)]][[1]]
  title_element = bulletin[[languaged_elements(language)]][[2]]
  
  expect_true(has_element(bulletin, type = "text"))
  expect_true(has_element(bulletin, type = "text"))
  expect_false(has_element(bulletin, type = "blabla"))
  expect_true(has_element(bulletin, id = title_element$id))
  expect_false(has_element(bulletin, id = "asdfasfd"))
  expect_error(has_element(bulletin, type = "asdf", id = "asdf"))
  expect_true(has_element(bulletin))
  
})

test_that("get_elements", {
  text <- "This is normal text."
  
  language = "de"
  
  bulletin <- create_bulletin(languages = language) %>%
    add_text(text) 
  
  text_element = bulletin[[languaged_elements(language)]][[1]]

  expect_equal(get_elements(bulletin, type = "text"), list(text_element), ignore_attr = TRUE)
  expect_equal(get_elements(bulletin, type = "blabla"), list(), ignore_attr = TRUE)
  expect_equal(get_elements(bulletin, id = text_element$id), list(text_element), ignore_attr = TRUE)
  expect_equal(get_elements(bulletin, id = "asdfasfd"), list(), ignore_attr = TRUE)
  expect_error(get_elements(bulletin, type = "asdf", id = "asdf"))
  expect_equal(get_elements(bulletin), bulletin$elements)
  
})

test_that("frontmatter", {
  bulletin <- create_bulletin(languages = "fr")
  filename <- tempfile()
  file_conn <- file(filename, open = "wb") # readr::write_lines only supports binary connections
  
  # write the R markdong front matter first
  write_markdown_frontmatter(bulletin = bulletin, file_conn = file_conn)
  close(file_conn)
  fm <- readr::read_lines(filename)
  expect_snapshot(fm)
})


