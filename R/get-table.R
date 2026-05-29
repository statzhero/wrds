#' Download data from any WRDS table
#'
#' Generic function to download data from any table in the WRDS database.
#' Returns a lazy table by default, allowing you to build queries with
#' dplyr before collecting.
#'
#' @param wrds A `DBIConnection` object returned by [wrds_connect()].
#' @param library Character. The name of the library (schema), e.g., `"crsp"`,
#'   `"comp"`, `"ibes"`.
#' @param table Character. The name of the table within the library.
#' @param columns Character vector of columns to return. If `NULL` (default),
#'   returns all columns. Use [describe_table()] to see available columns.
#' @param n Maximum number of rows to return. Defaults to `Inf` (all rows).
#'   Use a smaller value (e.g., `n = 100`) to preview data.
#' @param lazy If `TRUE` (default), returns a lazy `tbl` for further
#'   filtering with dplyr. Set to `FALSE` to collect immediately.
#'
#' @return A `tbl_lazy` object (if `lazy = TRUE`) or a tibble (if `lazy = FALSE`).
#'
#' @details
#' This function provides generic access to any WRDS table. For commonly-used
#' tables with standard research filters, prefer the specialized functions:
#' - [get_compustat()] for Compustat fundamentals with standard filters
#' - [get_company()] for company header data
#' - [link_ccm()] for CRSP-Compustat linking
#'
#' The lazy table can be filtered, selected, and mutated using dplyr verbs,
#' which are translated to SQL and executed on the server:
#'
#' ```r
#' get_table(wrds, "crsp", "msf") |>
#'   filter(date >= "2025-01-01") |>
#'   select(permno, date, ret, prc) |>
#'   collect()
#' ```
#'
#' @seealso [describe_table()] to explore table structure,
#'   [list_tables()] to list available tables in a library
#'
#' @export
#' @examples
#' \dontrun{
#' wrds <- wrds_connect()
#'
#' # Preview table structure first
#' describe_table(wrds, "crsp", "msf")
#'
#' # Get a lazy table and build your query
#' get_table(wrds, "crsp", "msf") |>
#'   dplyr::filter(date >= "2025-01-01") |>
#'   dplyr::select(permno, date, ret, prc, vol) |>
#'   dplyr::collect()
#'
#' # Collect immediately with specific columns
#' get_table(wrds, "crsp", "dsf",
#'   columns = c("permno", "date", "ret", "prc"),
#'   lazy = FALSE,
#'   n = 1000
#' )
#'
#' # Access any table in any library
#' get_table(wrds, "ibes", "statsum_epsus") |>
#'   dplyr::filter(fpedats >= "2025-01-01") |>
#'   dplyr::collect()
#'
#' wrds_disconnect(wrds)
#' }
get_table <- function(
  wrds,
  library,
  table,
  columns = NULL,
  n = Inf,
  lazy = TRUE
) {
  if (!is.character(library) || length(library) != 1) {
    cli::cli_abort("{.arg library} must be a single character string.")
  }

  if (!is.character(table) || length(table) != 1) {
    cli::cli_abort("{.arg table} must be a single character string.")
  }

  check_connection(wrds)

  tbl <- dplyr::tbl(wrds, DBI::Id(schema = library, table = table))

  if (!is.null(columns)) {
    tbl <- tbl |> dplyr::select(dplyr::any_of(columns))
  }

  if (is.finite(n)) {
    tbl <- utils::head(tbl, n)
  }

  smart_collect(tbl, wrds, lazy = lazy)
}
