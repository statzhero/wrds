#' Get CRSP-Compustat linking table
#'
#' Downloads the CCM (CRSP-Compustat Merged) linking table that maps
#' CRSP PERMNOs to Compustat GVKEYs with valid date ranges.
#'
#' @param wrds A `DBIConnection` object returned by [wrds_connect()].
#' @param linktype Character vector. Types of links to include.
#'   Defaults to `c("LC", "LU", "LS")`:
#'   - `"LC"`: Link confirmed by Compustat
#'   - `"LU"`: Link unconfirmed (valid but less certain)
#'   - `"LS"`: Link valid for secondary securities
#' @param linkprim Character vector. Link primacy filters.
#'   Defaults to `c("P", "C")`:
#'   - `"P"`: Primary link identified by Compustat
#'   - `"C"`: Primary link identified by CRSP
#' @param n Maximum number of rows to return. Defaults to `Inf` (all rows).
#'   Use a smaller value (e.g., `n = 100`) to preview data before downloading
#'   the full table.
#' @param lazy If `TRUE`, returns a lazy `tbl` instead of collecting.
#'   Defaults to `FALSE`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{gvkey}{Compustat company identifier}
#'   \item{permno}{CRSP permanent security identifier}
#'   \item{linkdt}{Start date of the link}
#'   \item{linkenddt}{End date of the link (missing values replaced with max date)}
#'   \item{linktype}{Type of link}
#'   \item{linkprim}{Link primacy}
#' }
#'
#' @details
#' The linking table comes from `crsp.ccmxpf_lnkhist`. Missing `linkenddt`
#' values indicate ongoing links and are replaced with the maximum date in the
#' table for easier date-range joins.
#'
#' To use the link, join on `gvkey` and ensure your observation date falls
#' within the `linkdt` to `linkenddt` range.
#'
#' @references
#' Ian Gow, *Financial Accounting Research*, Chapter on Identifiers:
#' <https://iangow.github.io/far_book/identifiers.html>
#'
#' @seealso [get_compustat()]
#'
#' @export
#' @examples
#' \dontrun{
#' wrds <- wrds_connect()
#' ccm <- link_ccm(wrds)
#'
#' # Join with Compustat data
#' compustat <- get_compustat(wrds)
#' compustat |>
#'   dplyr::inner_join(ccm, by = dplyr::join_by(gvkey)) |>
#'   dplyr::filter(datadate >= linkdt, datadate <= linkenddt)
#'
#' wrds_disconnect(wrds)
#' }
link_ccm <- function(
  wrds,
  linktype = c("LC", "LU", "LS"),
  linkprim = c("P", "C"),
  n = Inf,
  lazy = FALSE
) {
  check_connection(wrds)

  tbl <- dplyr::tbl(wrds, DBI::Id(schema = "crsp", table = "ccmxpf_lnkhist")) |>
    tidylog::filter(
      linktype %in% .env$linktype,
      linkprim %in% .env$linkprim
    ) |>
    tidylog::mutate(
      linkenddt = dplyr::coalesce(linkenddt, max(linkenddt, na.rm = TRUE))
    ) |>
    dplyr::select(
      "gvkey",
      permno = "lpermno",
      "linkdt",
      "linkenddt",
      "linktype",
      "linkprim"
    )

  if (is.finite(n)) {
    tbl <- utils::head(tbl, n)
  }

  smart_collect(tbl, wrds, lazy = lazy)
}

#' Get IBES-CRSP linking table
#'
#' Downloads the WRDS-provided linking table that maps IBES tickers to
#' CRSP PERMNOs with valid date ranges and match quality scores.
#'
#' @param wrds A `DBIConnection` object returned by [wrds_connect()].
#' @param max_score Maximum match quality score to include. Defaults to `5`,
#'   which excludes score 6 (the worst matches). Lower scores indicate
#'
#'   better matches:
#'   - `1`: Best match (CUSIP, ticker, and company name all match)
#'   - `2-5`: Progressively weaker matches
#'   - `6`: Worst match (excluded by default)
#' @param n Maximum number of rows to return. Defaults to `Inf` (all rows).
#'   Use a smaller value (e.g., `n = 100`) to preview data before downloading
#'   the full table.
#' @param lazy If `TRUE`, returns a lazy `tbl` instead of collecting.
#'   Defaults to `FALSE`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ticker}{IBES ticker}
#'   \item{permno}{CRSP permanent security identifier}
#'   \item{sdate}{Start date of the link}
#'   \item{edate}{End date of the link}
#'   \item{score}{Match quality score (1 = best, 6 = worst)}
#' }
#'
#' @details
#' The linking table comes from `wrdsapps_link_crsp_ibes.ibcrsphist`.
#'
#' To use the link, join on `ticker` and ensure your observation date falls
#' within the `sdate` to `edate` range.
#'
#' @references
#' WRDS IBES-CRSP Linking Table Documentation:
#' <https://wrds-www.wharton.upenn.edu/documents/796/IBES_CRSP_Linking_Table_by_WRDS.pdf>
#'
#' @seealso [link_ccm()]
#'
#' @export
#' @examples
#' \dontrun{
#' wrds <- wrds_connect()
#' ibes_link <- link_ibes_crsp(wrds)
#'
#' # Join with IBES data on ticker and date range
#' ibes_data |>
#'   dplyr::inner_join(ibes_link, by = dplyr::join_by(ticker)) |>
#'   dplyr::filter(date >= sdate, date <= edate)
#'
#' wrds_disconnect(wrds)
#' }
link_ibes_crsp <- function(wrds, max_score = 5L, n = Inf, lazy = FALSE) {
  check_connection(wrds)

  tbl <- dplyr::tbl(
    wrds,
    DBI::Id(schema = "wrdsapps_link_crsp_ibes", table = "ibcrsphist")
  ) |>
    tidylog::filter(score <= .env$max_score) |>
    dplyr::select("ticker", "permno", "sdate", "edate", "score")

  if (is.finite(n)) {
    tbl <- utils::head(tbl, n)
  }

  smart_collect(tbl, wrds, lazy = lazy)
}
