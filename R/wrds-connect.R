#' Connect to WRDS
#'
#' Establishes a connection to the WRDS PostgreSQL server using credentials
#' stored securely in the system keyring.
#'
#' @param user_key Name of the keyring entry storing the WRDS username.
#'   Defaults to `"wrds_user"`.
#' @param password_key Name of the keyring entry storing the WRDS password.
#'   Defaults to `"wrds_pw"`.
#' @param keyring Optional keyring name. If `NULL` (default), uses the
#'   default keyring.
#'
#' @return A `DBIConnection` object for the WRDS PostgreSQL database.
#'
#' @details
#' Credentials must be set up before first use with [wrds_set_credentials()].
#' The connection uses `bigint = "integer"` for compatibility with R's
#' integer type.
#'
#' @seealso [wrds_disconnect()], [wrds_set_credentials()]
#'
#' @export
#' @examples
#' \dontrun{
#' wrds <- wrds_connect()
#' list_libraries(wrds)
#' wrds_disconnect(wrds)
#' }
wrds_connect <- function(user_key = "wrds_user",
                         password_key = "wrds_pw",
                         keyring = NULL) {
  user <- tryCatch(
    keyring::key_get(user_key, keyring = keyring),
    error = \(e) {
      cli::cli_abort(c(
        "Could not retrieve WRDS username from keyring.",
        "i" = "Run {.fn wrds_set_credentials} to set up credentials."
      ))
    }
  )

  password <- tryCatch(
    keyring::key_get(password_key, keyring = keyring),
    error = \(e) {
      cli::cli_abort(c(
        "Could not retrieve WRDS password from keyring.",
        "i" = "Run {.fn wrds_set_credentials} to set up credentials."
      ))
    }
  )


  DBI::dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    port = 9737,
    dbname = "wrds",
    user = user,
    password = password,
    sslmode = "require",
    bigint = "integer"
  )
}

#' Disconnect from WRDS
#'
#' Closes a WRDS database connection.
#'
#' @param wrds A `DBIConnection` object returned by [wrds_connect()].
#'
#' @return Invisibly returns `TRUE` if disconnection was successful.
#'
#' @export
#' @examples
#' \dontrun{
#' wrds <- wrds_connect()
#' wrds_disconnect(wrds)
#' }
wrds_disconnect <- function(wrds) {
  check_connection(wrds)
  DBI::dbDisconnect(wrds)
  invisible(TRUE)
}

#' Set WRDS credentials
#'
#' Interactively stores WRDS username and password in the system keyring
#' for secure, persistent storage.
#'
#' @param user_key Name for the username keyring entry. Defaults to `"wrds_user"`.
#' @param password_key Name for the password keyring entry. Defaults to `"wrds_pw"`.
#' @param keyring Optional keyring name. If `NULL` (default), uses the
#'   default keyring.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @details
#' This function prompts for username and password interactively. Credentials
#' are stored securely using the operating system's keyring (Keychain on macOS,
#' Credential Manager on Windows, Secret Service on Linux).
#'
#' @export
#' @examples
#' \dontrun{
#' wrds_set_credentials()
#' }
wrds_set_credentials <- function(user_key = "wrds_user",
                                 password_key = "wrds_pw",
                                 keyring = NULL) {
  if (!interactive()) {
    cli::cli_abort("wrds_set_credentials() must be run interactively.")
  }

  cli::cli_alert_info("Setting WRDS credentials in system keyring.")

  user <- readline("WRDS username: ")
  if (nchar(user) == 0) {
    cli::cli_abort("Username cannot be empty.")
  }

  keyring::key_set_with_value(user_key, password = user, keyring = keyring)
  # This does not allow empty passwords
  keyring::key_set(password_key, keyring = keyring, prompt = "WRDS password: ")

  cli::cli_alert_success("Credentials stored successfully.")
  invisible(TRUE)
}
