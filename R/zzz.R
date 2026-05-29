.onLoad <- function(libname, pkgname) {
  op <- options()
  op_wrds <- list(
    wrds.collect_threshold = 200000L,
    wrds.abort_threshold = 2000000L
  )
  toset <- !(names(op_wrds) %in% names(op))
  if (any(toset)) options(op_wrds[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  header <- cli::rule(
    left = cli::style_bold("Checking WRDS credentials"),
    right = paste0(pkgname, " ", utils::packageVersion(pkgname))
  )
  packageStartupMessage(header)

  user <- tryCatch(
    keyring::key_get("wrds_user"),
    error = function(e) NULL
  )

  if (is.null(user)) {
    msg <- paste0(
      "\u26a0\ufe0f No credentials found. Run ",
      cli::col_cyan("wrds_set_credentials()"),
      " to configure."
    )
  } else {
    msg <- paste0(
      cli::col_green(cli::symbol$tick),
      " Credentials found for user ",
      cli::col_green(user)
    )
  }
  packageStartupMessage(msg)
}
