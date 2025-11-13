test_that("Set elements_dir to special location", {
  test_elements_dir = test_path("test_elements_dir")
  
  x <- 5 # used for webtest_element_de.Rmd
  
  bulletin <- create_minimal_bulletin(
    bulletin_id = "webtest",
    bulletin_args = list(Rmd_elements_dir = test_elements_dir)
  ) %>%
    set_active_language("de") %>%
    add_Rmd(element_id = "element")
  
  element <- bulletin$elements_de[[1]]
  expect_equal(element$id, "element")
  expect_equal(element$type, "Rmd")
})

test_that("Load Rmd from standard elements directory", {
  x <- 5 # used for webtest_element_de.Rmd
  
  bulletin <- create_minimal_bulletin(
    bulletin_id = "webtest"
  ) %>%
    set_active_language("de") %>%
    add_Rmd(element_id = "element")
  
  element <- bulletin$elements_de[[1]]
  expect_equal(element$id, "element")
  expect_equal(element$type, "Rmd")
})


test_that("Load Rmd from specified elements directory", {
  test_elements_dir = test_path("test_elements_dir")
  
  x <- 5 # used for webtest_element_de.Rmd
  
  bulletin <- create_minimal_bulletin(
    bulletin_id = "webtest"
  ) %>%
    set_active_language("de") %>%
    add_Rmd(element_id = "element",
            elements_dir = test_elements_dir)
  
  element <- bulletin$elements_de[[1]]
  expect_equal(element$id, "element")
  expect_equal(element$type, "Rmd")
})
