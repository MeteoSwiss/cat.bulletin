test_that("Create bulletin for web", {
  bulletin <- create_test_bulletin_for_web()
  
  expect_snapshot_file(file.path(bulletin$bulletin_path, "publication.xml"))
})