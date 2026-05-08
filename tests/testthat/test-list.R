# Integration tests

test_that("list_libraries returns character vector", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  libs <- list_libraries(wrds)
  expect_type(libs, "character")
  expect_true(length(libs) > 0)
  expect_true("comp" %in% libs)
  expect_true("crsp" %in% libs)
})

test_that("list_tables returns tables for comp library", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  tables <- list_tables(wrds, "comp")
  expect_type(tables, "character")
  expect_true("funda" %in% tables)
  expect_true("fundq" %in% tables)
})

test_that("describe_table returns metadata", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  desc <- describe_table(wrds, "comp", "funda")
  expect_type(desc, "list")

  expect_named(desc, c("columns", "nrow", "sample"))
  expect_s3_class(desc$columns, "data.frame")
  expect_true("gvkey" %in% desc$columns$column_name)
  expect_true("datadate" %in% desc$columns$column_name)
  expect_s3_class(desc$sample, "data.frame")
  expect_equal(nrow(desc$sample), 20)
})
