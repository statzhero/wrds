# Unit tests (no connection needed)

test_that("wrds_connect fails gracefully without credentials", {
  # Temporarily use non-existent key names

  expect_error(
    wrds_connect(user_key = "nonexistent_wrds_test_user"),
    "Could not retrieve WRDS username"
  )
})

test_that("wrds_set_credentials requires interactive session", {
  skip_if(interactive(), "Test only runs in non-interactive mode")
  expect_error(
    wrds_set_credentials(),
    "must be run interactively"
  )
})

test_that("wrds_update_password requires interactive session", {
  skip_if(interactive(), "Test only runs in non-interactive mode")
  expect_error(
    wrds_update_password(),
    "must be run interactively"
  )
})

# Integration tests (require credentials)

test_that("wrds_connect establishes valid connection", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  expect_s4_class(wrds, "PqConnection")
  expect_true(DBI::dbIsValid(wrds))
})

test_that("wrds_disconnect invalidates connection", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  wrds_disconnect(wrds)

  expect_false(DBI::dbIsValid(wrds))
})
