test_that("image element", {
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png")
  caption  <- "this is a caption"
  element <- image_element(filepath = filepath, caption = caption) 
  expect_equal(element$type, "image")
  expect_equal(element$caption, caption)
})
