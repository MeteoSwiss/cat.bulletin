test_that("table element", {
  filename_abs <- system.file("example-data", "bulletin_monthly", "monatsbilanz_temp", "climate-temperature-evolution-outlook_abs_1864-today_1991-2020_month_regSwiss_de.txt", package = "cat.bulletin")
  data_abs <- read.table(filename_abs, header = TRUE)
  
  data_abs <- data_abs[1:10,]
  myTable = flextable::flextable(data_abs)
  flextable_element <- flextable_element(flextable = myTable, bulletin_envir = environment())
  md <- flextable_to_markdown(flextable_element)
  # flextable id is exepcted to show up in markdown
  expect_match(md, flextable_element$id)
  # object with identifier flextable_id is expected to be stored in the environment
  expect_identical(get(envir=environment(), flextable_element$id), myTable)
})
