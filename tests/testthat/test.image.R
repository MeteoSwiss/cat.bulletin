test_that("image element", {
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png")
  caption  <- "this is a caption"
  element <- image_element(filename = basename(filepath), filepath = filepath, image_dir = "images", caption = caption, source = NULL, alt = NULL, label = NULL) 
  expect_equal(element$type, "image")
  expect_equal(element$caption, caption)
})


test_that("joining images", {
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png")
  outpath = file.path(tempdir(), "join_image_testoutput.png")
  join_images(
    image_filepaths = rep(filepath, 2),
    outpath = outpath
  )
  expect_snapshot_file(outpath)
})
