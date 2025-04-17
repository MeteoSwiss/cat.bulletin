test_that("Update metadata element", {
  m <- publication_metadata(path = "path1")
  m <- update_metadata_element(m, path = "/path2")
  expect_equal(m$path, "/path2")
  expect_warning(update_metadata_element(m, foo = "new element"))
})

test_that("Update multilanguage element", {
  s <- c(de = "as", fr = "bs")
  t <- update_multi_language_string(s, "de", "new")
  expect_equal(t, c(de = "new", fr = "bs"))
})


test_that("is_multi_language string", {
  expect_false(is_multi_language_string(c(de="a", "a", fr="a")))
  expect_true(is_multi_language_string(c(de="a", it = "a", fr="a"), 
                                       languages = c("de", "it", "fr"))) 
})