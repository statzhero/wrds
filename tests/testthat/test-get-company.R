# Unit tests

test_that("company_config returns correct table for NA", {
  config <- company_config("na")
  expect_equal(config$table, "company")
  expect_true("gvkey" %in% config$columns)
  expect_true("sic" %in% config$columns)
  expect_true("state" %in% config$columns)
})

test_that("company_config returns correct table for global", {
  config <- company_config("global")
  expect_equal(config$table, "g_company")
  expect_true("gvkey" %in% config$columns)
  expect_true("sic" %in% config$columns)
  expect_false("state" %in% config$columns)
})

# Integration tests

test_that("get_company returns NA data with expected columns", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  company <- get_company(wrds)

  expect_s3_class(company, "data.frame")
  expect_true(nrow(company) > 0)
  expect_true("gvkey" %in% names(company))
  expect_true("sic" %in% names(company))
  expect_true("naics" %in% names(company))
})

test_that("get_company returns global data", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  g_company <- get_company(wrds, region = "global")

  expect_s3_class(g_company, "data.frame")
  expect_true(nrow(g_company) > 0)
  expect_true("gvkey" %in% names(g_company))
  expect_true("loc" %in% names(g_company))
})

test_that("get_company lazy option works", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  tbl <- get_company(wrds, lazy = TRUE)

  expect_s3_class(tbl, "tbl_lazy")
})

test_that("get_company sic column is character", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  # sic in comp.company is stored as character (not integer like sich in funda)
  company <- get_company(wrds)

  expect_type(company$sic, "character")
})
