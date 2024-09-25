test_that("repeat expressions", {
  expect_error(retry(stop("error")))
  expect_equal(retry(expression(5+8)), 13)
})