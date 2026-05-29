#' Download Compustat company header data
#'
#' Downloads company-level static data from Compustat including header SIC
#' codes, NAICS codes, state of incorporation, and other identifying
#' information.
#'
#' @param wrds A `DBIConnection` object returned by [wrds_connect()].
#' @param region One of `"na"` (North America, default) or `"global"`.
#' @param columns Character vector of columns to return. Defaults to key
#'   identifiers and classification codes.
#' @param n Maximum number of rows to return. Defaults to `Inf` (all rows).
#'   Use a smaller value (e.g., `n = 100`) to preview data before downloading
#'   the full table.
#' @param lazy If `TRUE`, returns a lazy `tbl` instead of collecting.
#'   Defaults to `FALSE`.
#'
#' @return A tibble with company header data. Default columns vary by region:
#'
#' **North America** (from `comp.company`):
#' - `gvkey`, `conm`: Identifiers
#' - `sic`, `naics`: Industry classifications (character)
#' - `state`, `fic`, `loc`: Geographic information
#'
#' **Global** (from `comp.g_company`):
#' - `gvkey`, `conm`: Identifiers
#' - `sic`, `naics`: Industry classifications
#' - `fic`, `loc`: Geographic information
#'
#' @details
#' The `sic` column contains the "header" SIC code, which is the company's
#' most recent SIC classification stored as a character. For historical SIC
#' codes that change over time, use [get_compustat()] with `fill_sic = TRUE`,
#' which coalesces the historical `sich` (integer) with the header `sic`.
#'
#' @seealso [get_compustat()] for fundamentals data with optional SIC filling
#'
#' @export
#' @examples
#' \dontrun{
#' wrds <- wrds_connect()
#'
#' # Get company header data
#' company <- get_company(wrds)
#'
#' # Get global companies
#' g_company <- get_company(wrds, region = "global")
#'
#' # Lazy query
#' get_company(wrds, lazy = TRUE) |>
#'   dplyr::filter(sic == "7370") |>
#'   dplyr::collect()
#'
#' wrds_disconnect(wrds)
#' }
get_company <- function(
  wrds,
  region = c("na", "global"),
  columns = NULL,
  n = Inf,
  lazy = FALSE
) {
  check_connection(wrds)
  region <- rlang::arg_match(region)

  config <- company_config(region)

  tbl <- dplyr::tbl(wrds, DBI::Id(schema = "comp", table = config$table))

  if (!is.null(columns)) {
    selected_cols <- columns
  } else {
    selected_cols <- config$columns
  }

  tbl <- tbl |>
    dplyr::select(dplyr::any_of(selected_cols))

  if (is.finite(n)) {
    tbl <- utils::head(tbl, n)
  }

  smart_collect(tbl, wrds, lazy = lazy)
}

#' Get company table configuration
#'
#' Internal function returning table names and default columns for company
#' queries.
#'
#' @param region One of `"na"` or `"global"`.
#'
#' @return A list with `table` and `columns`.
#'
#' @keywords internal
company_config <- function(region) {
  if (region == "na") {
    list(
      table = "company",
      columns = c("gvkey", "conm", "sic", "naics", "state", "fic", "loc")
    )
  } else {
    list(
      table = "g_company",
      columns = c("gvkey", "conm", "sic", "naics", "fic", "loc")
    )
  }
}
