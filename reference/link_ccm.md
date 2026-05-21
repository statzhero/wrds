# Get CRSP-Compustat linking table

Downloads the CCM (CRSP-Compustat Merged) linking table that maps CRSP
PERMNOs to Compustat GVKEYs with valid date ranges.

## Usage

``` r
link_ccm(
  wrds,
  linktype = c("LC", "LU", "LS"),
  linkprim = c("P", "C"),
  n = Inf,
  lazy = FALSE
)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

- linktype:

  Character vector. Types of links to include. Defaults to
  `c("LC", "LU", "LS")`:

  - `"LC"`: Link confirmed by Compustat

  - `"LU"`: Link unconfirmed (valid but less certain)

  - `"LS"`: Link valid for secondary securities

- linkprim:

  Character vector. Link primacy filters. Defaults to `c("P", "C")`:

  - `"P"`: Primary link identified by Compustat

  - `"C"`: Primary link identified by CRSP

- n:

  Maximum number of rows to return. Defaults to `Inf` (all rows). Use a
  smaller value (e.g., `n = 100`) to preview data before downloading the
  full table.

- lazy:

  If `TRUE`, returns a lazy `tbl` instead of collecting. Defaults to
  `FALSE`.

## Value

A tibble with columns:

- gvkey:

  Compustat company identifier

- permno:

  CRSP permanent security identifier

- linkdt:

  Start date of the link

- linkenddt:

  End date of the link (missing values replaced with max date)

- linktype:

  Type of link

- linkprim:

  Link primacy

## Details

The linking table comes from `crsp.ccmxpf_lnkhist`. Missing `linkenddt`
values indicate ongoing links and are replaced with the maximum date in
the table for easier date-range joins.

To use the link, join on `gvkey` and ensure your observation date falls
within the `linkdt` to `linkenddt` range.

## References

Ian Gow, *Financial Accounting Research*, Chapter on Identifiers:
<https://iangow.github.io/far_book/identifiers.html>

## See also

[`get_compustat()`](https://statzhero.github.io/wrds/reference/get_compustat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
ccm <- link_ccm(wrds)

# Join with Compustat data
compustat <- get_compustat(wrds)
compustat |>
  dplyr::inner_join(ccm, by = dplyr::join_by(gvkey)) |>
  dplyr::filter(datadate >= linkdt, datadate <= linkenddt)

wrds_disconnect(wrds)
} # }
```
