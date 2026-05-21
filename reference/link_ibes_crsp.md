# Get IBES-CRSP linking table

Downloads the WRDS-provided linking table that maps IBES tickers to CRSP
PERMNOs with valid date ranges and match quality scores.

## Usage

``` r
link_ibes_crsp(wrds, max_score = 5L, n = Inf, lazy = FALSE)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

- max_score:

  Maximum match quality score to include. Defaults to `5`, which
  excludes score 6 (the worst matches). Lower scores indicate

  better matches:

  - `1`: Best match (CUSIP, ticker, and company name all match)

  - `2-5`: Progressively weaker matches

  - `6`: Worst match (excluded by default)

- n:

  Maximum number of rows to return. Defaults to `Inf` (all rows). Use a
  smaller value (e.g., `n = 100`) to preview data before downloading the
  full table.

- lazy:

  If `TRUE`, returns a lazy `tbl` instead of collecting. Defaults to
  `FALSE`.

## Value

A tibble with columns:

- ticker:

  IBES ticker

- permno:

  CRSP permanent security identifier

- sdate:

  Start date of the link

- edate:

  End date of the link

- score:

  Match quality score (1 = best, 6 = worst)

## Details

The linking table comes from `wrdsapps_link_crsp_ibes.ibcrsphist`.

To use the link, join on `ticker` and ensure your observation date falls
within the `sdate` to `edate` range.

## References

WRDS IBES-CRSP Linking Table Documentation:
<https://wrds-www.wharton.upenn.edu/documents/796/IBES_CRSP_Linking_Table_by_WRDS.pdf>

## See also

[`link_ccm()`](https://statzhero.github.io/wrds/reference/link_ccm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
ibes_link <- link_ibes_crsp(wrds)

# Join with IBES data on ticker and date range
ibes_data |>
  dplyr::inner_join(ibes_link, by = dplyr::join_by(ticker)) |>
  dplyr::filter(date >= sdate, date <= edate)

wrds_disconnect(wrds)
} # }
```
