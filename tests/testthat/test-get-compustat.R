# Unit tests

test_that("get_compustat rejects invalid frequency", {
  wrds <- mock_connection()
  local_mocked_bindings(
    dbIsValid = function(dbObj, ...) TRUE,
    .package = "DBI"
  )
  expect_error(
    get_compustat(wrds, frequency = "monthly"),
    "monthly"
  )
})

test_that("get_compustat rejects invalid region", {
  wrds <- mock_connection()
  local_mocked_bindings(
    dbIsValid = function(dbObj, ...) TRUE,
    .package = "DBI"
  )
  expect_error(
    get_compustat(wrds, region = "europe"),
    "europe"
  )
})

test_that("compustat_config returns correct table for NA annual", {
  config <- compustat_config("annual", "na")
  expect_equal(config$table, "funda")
  expect_equal(config$datafmt, "STD")
  expect_equal(config$popsrc, "D")
  expect_true("cusip" %in% config$columns)
  expect_true("tic" %in% config$columns)
})

test_that("compustat_config returns correct table for NA quarterly", {
  config <- compustat_config("quarterly", "na")
  expect_equal(config$table, "fundq")
  expect_true("fyearq" %in% config$columns)
  expect_true("fqtr" %in% config$columns)
})

test_that("compustat_config returns correct table for global annual", {
  config <- compustat_config("annual", "global")
  expect_equal(config$table, "g_funda")
  expect_equal(config$datafmt, "HIST_STD")
  expect_equal(config$popsrc, "I")
  expect_true("isin" %in% config$columns)
  expect_true("loc" %in% config$columns)
  expect_false("cusip" %in% config$columns)
})

test_that("compustat_config returns correct table for global quarterly", {
  config <- compustat_config("quarterly", "global")
  expect_equal(config$table, "g_fundq")
  expect_true("loc" %in% config$columns)
})

# Integration tests

test_that("get_compustat returns NA annual data", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  funda <- get_compustat(wrds,
    start_date = "2022-01-01",
    end_date = "2022-12-31"
  )

  expect_s3_class(funda, "data.frame")
  expect_true(nrow(funda) > 0)
  expect_true("gvkey" %in% names(funda))
  expect_true("cusip" %in% names(funda))
  expect_true("at" %in% names(funda))
})

test_that("get_compustat returns NA quarterly data", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  fundq <- get_compustat(wrds,
    frequency = "quarterly",
    start_date = "2022-01-01",
    end_date = "2022-03-31"
  )

  expect_s3_class(fundq, "data.frame")
  expect_true(nrow(fundq) > 0)
  expect_true("fyearq" %in% names(fundq))
  expect_true("fqtr" %in% names(fundq))
})

test_that("get_compustat returns global data", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  g_funda <- get_compustat(wrds,
    region = "global",
    start_date = "2022-01-01",
    end_date = "2022-12-31"
  )

  expect_s3_class(g_funda, "data.frame")
  expect_true(nrow(g_funda) > 0)
  expect_true("isin" %in% names(g_funda))
  expect_true("loc" %in% names(g_funda))
})

test_that("get_compustat lazy option works", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  tbl <- get_compustat(wrds, lazy = TRUE)

  expect_s3_class(tbl, "tbl_lazy")
})

test_that("get_compustat accepts additional columns", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  # emp (employees) is not in defaults but exists in funda
  funda <- get_compustat(wrds,
    start_date = "2022-01-01",
    end_date = "2022-12-31",
    add_columns = "emp"
  )

  expect_true("emp" %in% names(funda))
})

# fill_sic tests

test_that("fill_sic alerts for global region", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  expect_message(
    get_compustat(wrds,
      region = "global",
      start_date = "2022-01-01",
      end_date = "2022-12-31",
      fill_sic = TRUE
    ),
    "only supported for North America",
    class = "cliMessage"
  )
})

test_that("fill_sic fills missing SIC codes", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  funda <- get_compustat(wrds,
    start_date = "2022-01-01",
    end_date = "2022-12-31",
    fill_sic = TRUE
  )

  expect_true("sic" %in% names(funda))
  expect_false("sich" %in% names(funda))
  expect_type(funda$sic, "character")
})

test_that("fill_sic with lazy returns sich column and message", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  expect_message(
    tbl <- get_compustat(wrds, fill_sic = TRUE, lazy = TRUE),
    "requires collection"
  )

  expect_s3_class(tbl, "tbl_lazy")
  expect_true("sich" %in% colnames(tbl))
})

test_that("fill_sic reduces NA count compared to sich alone", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  # Get data with sich only
  funda_sich <- get_compustat(wrds,
    start_date = "2022-01-01",
    end_date = "2022-12-31",
    add_columns = "sich"
  )

 # Get data with fill_sic
  funda_filled <- get_compustat(wrds,
    start_date = "2022-01-01",
    end_date = "2022-12-31",
    fill_sic = TRUE
  )

  # Filled sic should have equal or fewer NAs than sich
  expect_lte(
    sum(is.na(funda_filled$sic)),
    sum(is.na(funda_sich$sich))
  )
})

# sic_2digit tests

test_that("sic_2digit extracts first two characters", {
  expect_equal(sic_2digit(5812), "58")
  expect_equal(sic_2digit(c(5812, 5813, 7011)), c("58", "58", "70"))
})

test_that("sic_2digit handles NA and edge cases", {
  expect_equal(sic_2digit(NA_integer_), NA_character_)
  expect_equal(sic_2digit(100), "10")
  expect_equal(sic_2digit(9999), "99")
})
