test_that("download realization works", {
  testthat::skip_on_ci()

  file <- download_realization(
      bulletin = create_bulletin(),
      product = "climate-precipitation-maps-Ynorm",
      filter = list(
        "valueBase" = "diff",
        "timeGranularity" = "Y",
        "parameter" = "R",
        "normalPeriod" = "1991-2020",
        "mediaType" = "image/png",
        "representation" = "nostats",
        "productName" = "climate-precipitation-maps-Ynorm"),
      filename = "test.png"
    )
  expect_equal(grep("test.png", file), 1)
})
