#' Download Compustat fundamentals
#'
#' Downloads financial statement data from Compustat with standard filters
#' for clean, analysis-ready data.
#'
#' @param wrds A `DBIConnection` object returned by [wrds_connect()].
#' @param frequency One of `"annual"` (default) or `"quarterly"`.
#' @param region One of `"na"` (North America, default) or `"global"`.
#' @param start_date Start date for filtering. Character string in
#'   `"YYYY-MM-DD"` format or a Date object. Defaults to `NULL` (no filter).
#' @param end_date End date for filtering. Character string in
#'   `"YYYY-MM-DD"` format or a Date object. Defaults to `NULL` (no filter).
#' @param columns Character vector of columns to return, replacing the defaults.
#'   Use [describe_table()] to see available columns.
#' @param add_columns Character vector of additional columns to include beyond
#'   the defaults. Ignored if `columns` is specified.
#' @param indfmt Industry format filter. Defaults to `"INDL"` (industrial).
#'   Use `"FS"` for financial services format.
#' @param consol Consolidation level. Defaults to `"C"` (consolidated).
#'   Use `"B"` for both consolidated and non-consolidated.
#' @param fill_sic If `TRUE`, fills missing historical SIC codes (`sich`) with
#'   header SIC codes from `comp.company`. Only supported for North America.
#'   When used with `lazy = TRUE`, returns the table with `sich` but without
#'   the join (requires manual joining with [get_company()]). Defaults to
#'   `FALSE`.
#' @param n Maximum number of rows to return. Defaults to `Inf` (all rows).
#'   Use a smaller value (e.g., `n = 100`) to preview data before downloading
#'   the full table.
#' @param lazy If `TRUE`, returns a lazy `tbl` instead of collecting.
#'   Defaults to `FALSE`.
#'
#' @return A tibble with Compustat fundamentals. Default columns vary by region:
#'
#' **North America** (from `comp.funda` / `comp.fundq`):
#' - Identifiers: `gvkey`, `cusip`, `tic`, `conm`, `datadate`
#' - Time: `fyear`/`fyearq`, `fyr`/`fqtr`
#' - Income: `ni`/`niq`, `ib`/`ibq`, `oiadp`/`oiadpq`, `revt`/`revtq`
#' - Balance sheet: `at`/`atq`, `lt`/`ltq`, `seq`/`seqq`, `ceq`/`ceqq`
#' - Market: `csho`/`cshoq`, `prcc_f`/`prccq`
#' - Other: `sale`/`saleq`, `capx`/`capxy`, `che`/`cheq`, `dlc`/`dlcq`, `dltt`/`dlttq`
#' - Industry: `sich` (historical SIC); `sic` (when `fill_sic = TRUE`, coalesced
#'   from `sich` and header SIC)
#'
#' **Global** (from `comp.g_funda` / `comp.g_fundq`):
#' - Identifiers: `gvkey`, `isin`, `conm`, `datadate`
#' - Geography: `loc`, `fic`, `exchg`
#' - Similar financial variables (with some differences, e.g., `nit`/`nitq`
#'   instead of `ni`/`niq`)
#'
#' @details
#' Default filters follow standard practice for most research applications.
#' Region-specific filters are applied automatically based on `region`:
#' - `datafmt`: `"STD"` for North America, `"HIST_STD"` for Global
#' - `popsrc`: `"D"` (domestic) for North America, `"I"` (international) for Global
#'
#' North America and Global data have different structures and should not
#' be combined without careful column harmonization.
#'
#' @seealso [link_ccm()] for CRSP-Compustat linking, [get_company()] for
#'   company header data
#'
#' @export
#' @examples
#' \dontrun{
#' wrds <- wrds_connect()
#'
#' # Annual North America fundamentals
#' funda <- get_compustat(wrds)
#'
#' # Quarterly with date filter
#' fundq <- get_compustat(wrds,
#'   frequency = "quarterly",
#'   start_date = "2020-01-01",
#'   end_date = "2023-12-31"
#' )
#'
#' # Global annual
#' g_funda <- get_compustat(wrds, region = "global")
#'
#' # Lazy query for further filtering
#' get_compustat(wrds, lazy = TRUE) |>
#'   dplyr::filter(fyear >= 2020) |>
#'   dplyr::select(gvkey, datadate, at, lt) |>
#'   dplyr::collect()
#'
#' # Fill missing SIC codes with header SIC from comp.company
#' funda_sic <- get_compustat(wrds, fill_sic = TRUE)
#'
#' # Preview first 100 rows before full download
#' preview <- get_compustat(wrds, n = 100)
#'
#' wrds_disconnect(wrds)
#' }
get_compustat <- function(
  wrds,
  frequency = c("annual", "quarterly"),
  region = c("na", "global"),
  start_date = NULL,
  end_date = NULL,
  columns = NULL,
  add_columns = NULL,
  indfmt = "INDL",
  consol = "C",
  fill_sic = FALSE,
  n = Inf,
  lazy = FALSE
) {
  check_connection(wrds)
  frequency <- rlang::arg_match(frequency)
  region <- rlang::arg_match(region)

  if (fill_sic && region == "global") {
    cli::cli_alert_warning(
      "{.arg fill_sic} is only supported for North America. Ignoring."
    )
    fill_sic <- FALSE
  }

  config <- compustat_config(frequency, region)

  tbl <- dplyr::tbl(wrds, DBI::Id(schema = "comp", table = config$table)) |>
    dplyr::filter(
      indfmt == .env$indfmt,
      consol == .env$consol,
      datafmt == .env$config$datafmt,
      popsrc == .env$config$popsrc
    )

  if (!is.null(start_date)) {
    tbl <- tbl |> dplyr::filter(datadate >= .env$start_date)
  }

  if (!is.null(end_date)) {
    tbl <- tbl |> dplyr::filter(datadate <= .env$end_date)
  }

  if (!is.null(columns)) {
    selected_cols <- columns
  } else {
    selected_cols <- config$columns
    if (!is.null(add_columns)) {
      selected_cols <- unique(c(selected_cols, add_columns))
    }
  }

  # Ensure sich is included when fill_sic is requested
  if (fill_sic && is.null(columns)) {
    selected_cols <- unique(c(selected_cols, "sich"))
  }

  tbl <- tbl |>
    dplyr::select(dplyr::any_of(selected_cols))

  if (is.finite(n)) {
    tbl <- utils::head(tbl, n)
  }

  if (fill_sic) {
    if (lazy) {
      cli::cli_inform(c(
        "i" = "SIC filling with {.arg fill_sic} = {.code TRUE} requires collection.",
        "i" = "Returning lazy table with {.field sich} column.",
        "i" = "Join with {.fn get_company} manually, or use {.arg lazy} = {.code FALSE}."
      ))
      result <- tbl
    } else {
      result <- fill_sic_codes(tbl, wrds)
    }
  } else {
    result <- smart_collect(tbl, wrds, lazy = lazy)
  }

  if (!lazy) {
    result <- dplyr::arrange(result, gvkey, datadate)
  }

  result
}

#' Get Compustat configuration
#'
#' Internal function returning table names and filters for Compustat queries.
#'
#' @param frequency One of `"annual"` or `"quarterly"`.
#' @param region One of `"na"` or `"global"`.
#'
#' @return A list with `table`, `datafmt`, `popsrc`, and `columns`.
#'
#' @keywords internal
compustat_config <- function(frequency, region) {
  if (region == "na") {
    if (frequency == "annual") {
      list(
        table = "funda",
        datafmt = "STD",
        popsrc = "D",
        columns = c(
          "gvkey",
          "cusip",
          "tic",
          "conm",
          "datadate",
          "fyear",
          "fyr",
          "ni",
          "ib",
          "oiadp",
          "revt",
          "at",
          "lt",
          "seq",
          "ceq",
          "csho",
          "prcc_f",
          "sale",
          "cogs",
          "xsga",
          "capx",
          "xrd",
          "che",
          "dlc",
          "dltt",
          "re",
          "invt",
          "ppent",
          "curcd",
          "sich"
        )
      )
    } else {
      list(
        table = "fundq",
        datafmt = "STD",
        popsrc = "D",
        columns = c(
          "gvkey",
          "cusip",
          "tic",
          "conm",
          "datadate",
          "fyearq",
          "fqtr",
          "niq",
          "ibq",
          "oiadpq",
          "revtq",
          "atq",
          "ltq",
          "seqq",
          "ceqq",
          "cshoq",
          "prccq",
          "mkvaltq",
          "saleq",
          "cogsq",
          "xsgaq",
          "capxy",
          "xrdq",
          "cheq",
          "dlcq",
          "dlttq",
          "req",
          "invtq",
          "ppentq",
          "curcdq",
          "datacqtr",
          "datafqtr",
          "sich"
        )
      )
    }
  } else {
    if (frequency == "annual") {
      list(
        table = "g_funda",
        datafmt = "HIST_STD",
        popsrc = "I",
        columns = c(
          "gvkey",
          "isin",
          "conm",
          "datadate",
          "fyear",
          "fyr",
          "nit",
          "ib",
          "oiadp",
          "revt",
          "at",
          "lt",
          "seq",
          "ceq",
          "sale",
          "cogs",
          "xsga",
          "capx",
          "che",
          "dlc",
          "dltt",
          "rect",
          "invt",
          "ppent",
          "curcd",
          "loc",
          "fic",
          "exchg"
        )
      )
    } else {
      list(
        table = "g_fundq",
        datafmt = "HIST_STD",
        popsrc = "I",
        columns = c(
          "gvkey",
          "isin",
          "conm",
          "datadate",
          "fyearq",
          "fqtr",
          "nitq",
          "ibq",
          "oiadpq",
          "revtq",
          "atq",
          "ltq",
          "seqq",
          "ceqq",
          "saleq",
          "cogsq",
          "xsgaq",
          "capxy",
          "cheq",
          "dlcq",
          "dlttq",
          "rectq",
          "invtq",
          "ppentq",
          "curcdq",
          "datacqtr",
          "datafqtr",
          "loc",
          "fic",
          "exchg"
        )
      )
    }
  }
}

#' Fill missing SIC codes from company header
#'
#' Internal function that joins fundamentals data with company header
#' to fill missing historical SIC codes using coalesce.
#'
#' @param tbl A lazy table from funda/fundq.
#' @param wrds Database connection.
#'
#' @return A collected tibble with `sic` column (character, coalesced from
#'   `sich` and header `sic`).
#'
#' @keywords internal
fill_sic_codes <- function(tbl, wrds) {
  company_sic <- dplyr::tbl(
    wrds,
    DBI::Id(schema = "comp", table = "company")
  ) |>
    dplyr::select(gvkey, sic_header = sic)

  tbl |>
    dplyr::mutate(sich = as.character(sich)) |>
    dplyr::left_join(company_sic, by = dplyr::join_by(gvkey)) |>
    dplyr::mutate(sic = dplyr::coalesce(sich, sic_header)) |>
    dplyr::select(-sic_header, -sich) |>
    dplyr::collect()
}

#' Convert SIC codes to 2-digit industry codes
#'
#' Extracts the first two characters from SIC codes to create broader
#' industry classifications.
#'
#' @param sic A numeric or character vector of SIC codes.
#'
#' @return A character vector of 2-digit SIC codes.
#'
#' @details
#' SIC codes are hierarchical: the first two digits represent major industry
#' groups (e.g., "54" = Retail-Food Stores), while the full 4-digit code
#' provides more specific classifications (e.g., "5412" = Retail-Convenience Stores).
#'
#' @export
#' @examples
#' # Convenience Stores (SIC 5412) -> Retail-Food Stores (54)
#' sic_2digit(5412)
#' # [1] "54"
#'
#' sic_2digit(c(5412, 5400))
#' # [1] "54" "54"
sic_2digit <- function(sic) {
  substr(as.character(sic), 1, 2)
}
