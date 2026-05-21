# Download Compustat fundamentals

Downloads financial statement data from Compustat with standard filters
for clean, analysis-ready data.

## Usage

``` r
get_compustat(
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
)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

- frequency:

  One of `"annual"` (default) or `"quarterly"`.

- region:

  One of `"na"` (North America, default) or `"global"`.

- start_date:

  Start date for filtering. Character string in `"YYYY-MM-DD"` format or
  a Date object. Defaults to `NULL` (no filter).

- end_date:

  End date for filtering. Character string in `"YYYY-MM-DD"` format or a
  Date object. Defaults to `NULL` (no filter).

- columns:

  Character vector of columns to return, replacing the defaults. Use
  [`describe_table()`](https://statzhero.github.io/wrds/reference/describe_table.md)
  to see available columns.

- add_columns:

  Character vector of additional columns to include beyond the defaults.
  Ignored if `columns` is specified.

- indfmt:

  Industry format filter. Defaults to `"INDL"` (industrial). Use `"FS"`
  for financial services format.

- consol:

  Consolidation level. Defaults to `"C"` (consolidated). Use `"B"` for
  both consolidated and non-consolidated.

- fill_sic:

  If `TRUE`, fills missing historical SIC codes (`sich`) with header SIC
  codes from `comp.company`. Only supported for North America. When used
  with `lazy = TRUE`, returns the table with `sich` but without the join
  (requires manual joining with
  [`get_company()`](https://statzhero.github.io/wrds/reference/get_company.md)).
  Defaults to `FALSE`.

- n:

  Maximum number of rows to return. Defaults to `Inf` (all rows). Use a
  smaller value (e.g., `n = 100`) to preview data before downloading the
  full table.

- lazy:

  If `TRUE`, returns a lazy `tbl` instead of collecting. Defaults to
  `FALSE`.

## Value

A tibble with Compustat fundamentals. Default columns vary by region:

**North America** (from `comp.funda` / `comp.fundq`):

- Identifiers: `gvkey`, `cusip`, `tic`, `conm`, `datadate`

- Time: `fyear`/`fyearq`, `fyr`/`fqtr`

- Income: `ni`/`niq`, `ib`/`ibq`, `oiadp`/`oiadpq`, `revt`/`revtq`

- Balance sheet: `at`/`atq`, `lt`/`ltq`, `seq`/`seqq`, `ceq`/`ceqq`

- Market: `csho`/`cshoq`, `prcc_f`/`prccq`

- Other: `sale`/`saleq`, `capx`/`capxy`, `che`/`cheq`, `dlc`/`dlcq`,
  `dltt`/`dlttq`

- Industry: `sich` (historical SIC); `sic` (when `fill_sic = TRUE`,
  coalesced from `sich` and header SIC)

**Global** (from `comp.g_funda` / `comp.g_fundq`):

- Identifiers: `gvkey`, `isin`, `conm`, `datadate`

- Geography: `loc`, `fic`, `exchg`

- Similar financial variables (with some differences, e.g., `nit`/`nitq`
  instead of `ni`/`niq`)

## Details

Default filters follow standard practice for most research applications.
Region-specific filters are applied automatically based on `region`:

- `datafmt`: `"STD"` for North America, `"HIST_STD"` for Global

- `popsrc`: `"D"` (domestic) for North America, `"I"` (international)
  for Global

North America and Global data have different structures and should not
be combined without careful column harmonization.

## See also

[`link_ccm()`](https://statzhero.github.io/wrds/reference/link_ccm.md)
for CRSP-Compustat linking,
[`get_company()`](https://statzhero.github.io/wrds/reference/get_company.md)
for company header data

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()

# Annual North America fundamentals
funda <- get_compustat(wrds)

# Quarterly with date filter
fundq <- get_compustat(wrds,
  frequency = "quarterly",
  start_date = "2020-01-01",
  end_date = "2023-12-31"
)

# Global annual
g_funda <- get_compustat(wrds, region = "global")

# Lazy query for further filtering
get_compustat(wrds, lazy = TRUE) |>
  dplyr::filter(fyear >= 2020) |>
  dplyr::select(gvkey, datadate, at, lt) |>
  dplyr::collect()

# Fill missing SIC codes with header SIC from comp.company
funda_sic <- get_compustat(wrds, fill_sic = TRUE)

# Preview first 100 rows before full download
preview <- get_compustat(wrds, n = 100)

wrds_disconnect(wrds)
} # }
```
