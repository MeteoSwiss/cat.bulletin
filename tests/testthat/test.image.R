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

test_that("get image size", {
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png")
  size <- get_image_size(filepath)
  expect_equal(size, expected = c(width = 810, height = 560))
})

test_that("crop image", {
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png")
  margin <- 20
  outpath <- crop_image(filepath, 
                        outpath = tempfile(fileext = ".png"), 
                        side = "top",
                        margin = margin)
  
  size_in <- get_image_size(filepath)
  size_out <- get_image_size(outpath)
  expect_equal(size_out["height"], size_in["height"] - margin)
})

test_that("resize image", {
  filepath <- system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_regSwiss_fr.png")
  
  # Resize to half the width, keeping aspect ratio
  outpath <- resize_image(
    image_filepath = filepath,
    outpath = tempfile(fileext = ".png"),
    width = "50%"
  )
  
  size_in <- get_image_size(filepath)
  size_out <- get_image_size(outpath)
  
  # Expect the width to be halved
  expect_equal(size_out["width"], round(size_in["width"] * 0.5))
  
  # Height should be proportional
  expected_height <- round(size_in["height"] * 0.5)
  expect_equal(size_out["height"], expected_height)
})