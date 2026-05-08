# Integration test for smart_collect abort threshold with real CRSP data

test_that("smart_collect aborts for daily CRSP without filters", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  # Daily CRSP stock file has 100M+ rows - should trigger abort

  crsp_daily <- dplyr::tbl(wrds, DBI::Id(schema = "crsp", table = "dsf"))

  expect_error(
    smart_collect(crsp_daily, wrds),
    class = "rlang_error"
  )
})
