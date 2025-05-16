
test_that("read_shorties", {
  shorties_path = system.file("example-data", "shorties", package = "cat.bulletin")
  
  shorties <- read_shorties(group_id = "event", language = "de", path = shorties_path)
  expect_equal(length(shorties), 4)
  expect_false(is.null(shorties[[1]]$pdf_file))
  
  shorties <- read_shorties(group_id = "event", language = "fr", path = shorties_path)
  expect_equal(length(shorties), 0)
  
  shorties <- read_shorties(group_id = "event", language = "it", path = shorties_path)
  expect_equal(length(shorties), 2)
  
})

test_that("shorties pdf", {
  bulletin <- create_test_bulletin_for_web()
  
  bulletin <- set_active_language(bulletin, "de")
  expect_true(has_element(bulletin, type = "shorties_list"))
  shorties_list <- get_elements(bulletin, type = "shorties_list")[[1]]
  pdfs <- get_shorties_pdfs(shorties_list)
  expect_type(pdfs, "character")
  expect_equal(length(pdfs), 1)
  
  bulletin <- set_active_language(bulletin, "it")
  expect_true(has_element(bulletin, type = "shorties_list"))
  shorties_list <- get_elements(bulletin, type = "shorties_list")[[1]]
  pdfs <- get_shorties_pdfs(shorties_list)
  expect_type(pdfs, "character")
  expect_equal(length(pdfs), 2)
})
