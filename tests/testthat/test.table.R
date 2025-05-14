test_that("flextable element", {
  filename_abs <- system.file("example-data", "bulletin_monthly","evolution-outlook_anom_regSwiss_de.txt", package = "cat.bulletin")
  data_abs <- read.table(filename_abs, header = TRUE)
  
  data_abs <- data_abs[1:10,]
  table_element <- flextable_element(flextable::flextable(data_abs), bulletin_envir = environment())
  md <- flextable_to_markdown(table_element)
  expect_match(md, "r, echo=FALSE}\nflextable")
})
