test_that("Create bulletin for web", {
  bulletin <- create_test_bulletin_for_web()
  webzip <- bulletin_to_webzip(bulletin)
  expect_snapshot_file(file.path(bulletin$bulletin_path, name = "publication.xml"))
})