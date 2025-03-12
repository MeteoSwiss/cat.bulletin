test_that("get_final_date", {
  skip_on_ci() # locales?
  expect_equal(get_final_date(2024, 5, "de"), "31. Mai 2024")
  expect_equal(get_final_date(2024, 9, "fr"), "30. septembre 2024")
  expect_equal(get_final_date(2024, 5, "it"), "31. maggio 2024")
})                                         
