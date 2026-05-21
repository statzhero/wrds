# Download Compustat company header data

Downloads company-level static data from Compustat including header SIC
codes, NAICS codes, state of incorporation, and other identifying
information.

## Usage

``` r
get_company(
  wrds,
  region = c("na", "global"),
  columns = NULL,
  n = Inf,
  lazy = FALSE
)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

- region:

  One of `"na"` (North America, default) or `"global"`.

- columns:

  Character vector of columns to return. Defaults to key identifiers and
  classification codes.

- n:

  Maximum number of rows to return. Defaults to `Inf` (all rows). Use a
  smaller value (e.g., `n = 100`) to preview data before downloading the
  full table.

- lazy:

  If `TRUE`, returns a lazy `tbl` instead of collecting. Defaults to
  `FALSE`.

## Value

A tibble with company header data. Default columns vary by region:

**North America** (from `comp.company`):

- `gvkey`, `conm`: Identifiers

- `sic`, `naics`: Industry classifications (character)

- `state`, `fic`, `loc`: Geographic information

**Global** (from `comp.g_company`):

- `gvkey`, `conm`: Identifiers

- `sic`, `naics`: Industry classifications

- `fic`, `loc`: Geographic information

## Details

The `sic` column contains the "header" SIC code, which is the company's
most recent SIC classification stored as a character. For historical SIC
codes that change over time, use
[`get_compustat()`](https://statzhero.github.io/wrds/reference/get_compustat.md)
with `fill_sic = TRUE`, which coalesces the historical `sich` (integer)
with the header `sic`.

## See also

[`get_compustat()`](https://statzhero.github.io/wrds/reference/get_compustat.md)
for fundamentals data with optional SIC filling

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()

# Get company header data
company <- get_company(wrds)

# Get global companies
g_company <- get_company(wrds, region = "global")

# Lazy query
get_company(wrds, lazy = TRUE) |>
  dplyr::filter(sic == "7370") |>
  dplyr::collect()

wrds_disconnect(wrds)
} # }
```
