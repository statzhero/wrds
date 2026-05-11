# Integration tests

test_that("link_ccm returns expected columns", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  # Use lazy to avoid downloading full table
  ccm <- link_ccm(wrds, lazy = TRUE)

  cols <- colnames(ccm)
  expect_true("gvkey" %in% cols)
  expect_true("permno" %in% cols)
  expect_true("linkdt" %in% cols)
  expect_true("linkenddt" %in% cols)
})

test_that("link_ccm filters by linktype and linkprim", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  # Get a small sample with specific filters
  ccm <- link_ccm(wrds, linktype = "LC", linkprim = "P", lazy = TRUE) |>
    dplyr::filter(linkdt >= "2020-01-01") |>
    dplyr::collect()

  expect_true(all(ccm$linktype == "LC"))
  expect_true(all(ccm$linkprim == "P"))
})

test_that("link_ccm handles missing linkenddt", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  ccm <- link_ccm(wrds, lazy = TRUE) |>
    dplyr::filter(linkdt >= "2020-01-01") |>
    dplyr::collect()

  # No missing end dates after coalesce
  expect_false(any(is.na(ccm$linkenddt)))
})

# link_ibes_crsp tests

test_that("link_ibes_crsp returns expected columns", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  subs <- list_subscriptions(wrds)
  skip_if(!("wrdsapps_link_crsp_ibes" %in% subs),
          "No subscription to IBES-CRSP linking table")

  ibes <- link_ibes_crsp(wrds, lazy = TRUE)

  cols <- colnames(ibes)
  expect_true("ticker" %in% cols)
  expect_true("permno" %in% cols)
  expect_true("sdate" %in% cols)
  expect_true("edate" %in% cols)
  expect_true("score" %in% cols)
})

test_that("link_ibes_crsp filters by max_score", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  subs <- list_subscriptions(wrds)
  skip_if(!("wrdsapps_link_crsp_ibes" %in% subs),
          "No subscription to IBES-CRSP linking table")

  # Default excludes score 6
  ibes <- link_ibes_crsp(wrds, lazy = TRUE) |>
    dplyr::filter(sdate >= "2020-01-01") |>
    dplyr::collect()

  expect_true(all(ibes$score <= 5))

  # Stricter filter
  ibes_strict <- link_ibes_crsp(wrds, max_score = 2L, lazy = TRUE) |>
    dplyr::filter(sdate >= "2020-01-01") |>
    dplyr::collect()

  expect_true(all(ibes_strict$score <= 2))
})
