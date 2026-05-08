# Unit tests (no connection needed)

# -- check_connection ---------------------------------------------------------

test_that("check_connection rejects non-DBIConnection objects", {
  expect_error(
    check_connection("not a connection"),
    "must be a database connection"

  )
  expect_error(
    check_connection(42),
    "must be a database connection"
  )
})

test_that("check_connection rejects invalid (closed) connections", {
  mock_conn <- mock_connection()
  local_mocked_bindings(
    dbIsValid = function(dbObj, ...) FALSE,
    .package = "DBI"
  )
  expect_error(
    check_connection(mock_conn),
    "no longer valid"
  )
})

# -- wrds_connect -------------------------------------------------------------

test_that("wrds_connect fails gracefully without username", {
  expect_error(
    wrds_connect(user_key = "nonexistent_wrds_test_user"),
    "Could not retrieve WRDS username"
  )
})

test_that("wrds_connect fails gracefully without password", {
  local_mocked_bindings(
    key_get = function(service, ...) {
      if (service == "wrds_user") return("fake_user")
      stop("key not found")
    },
    .package = "keyring"
  )
  expect_error(
    wrds_connect(),
    "Could not retrieve WRDS password"
  )
})

test_that("wrds_connect wraps PAM authentication errors", {
  local_mocked_bindings(
    key_get = function(service, ...) {
      if (service == "wrds_user") return("fake_user")
      if (service == "wrds_pw") return("fake_pw")
    },
    .package = "keyring"
  )
  local_mocked_bindings(
    dbConnect = function(...) {
      stop("PAM authentication failed for user \"fake_user\"")
    },
    .package = "DBI"
  )
  expect_error(
    wrds_connect(),
    "authentication failed"
  )
})

test_that("wrds_connect wraps generic connection errors", {
  local_mocked_bindings(
    key_get = function(service, ...) {
      if (service == "wrds_user") return("fake_user")
      if (service == "wrds_pw") return("fake_pw")
    },
    .package = "keyring"
  )
  local_mocked_bindings(
    dbConnect = function(...) stop("connection refused"),
    .package = "DBI"
  )
  expect_error(
    wrds_connect(),
    "Failed to connect to WRDS"
  )
})

# -- wrds_set_credentials / wrds_update_password ------------------------------

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
