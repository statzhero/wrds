# Download data from any WRDS table

Generic function to download data from any table in the WRDS database.
Returns a lazy table by default, allowing you to build queries with
dplyr before collecting.

## Usage

``` r
get_table(wrds, library, table, columns = NULL, n = Inf, lazy = TRUE)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

- library:

  Character. The name of the library (schema), e.g., `"crsp"`, `"comp"`,
  `"ibes"`.

- table:

  Character. The name of the table within the library.

- columns:

  Character vector of columns to return. If `NULL` (default), returns
  all columns. Use
  [`describe_table()`](https://statzhero.github.io/wrds/reference/describe_table.md)
  to see available columns.

- n:

  Maximum number of rows to return. Defaults to `Inf` (all rows). Use a
  smaller value (e.g., `n = 100`) to preview data.

- lazy:

  If `TRUE` (default), returns a lazy `tbl` for further filtering with
  dplyr. Set to `FALSE` to collect immediately.

## Value

A `tbl_lazy` object (if `lazy = TRUE`) or a tibble (if `lazy = FALSE`).

## Details

This function provides generic access to any WRDS table. For
commonly-used tables with standard research filters, prefer the
specialized functions:

- [`get_compustat()`](https://statzhero.github.io/wrds/reference/get_compustat.md)
  for Compustat fundamentals with standard filters

- [`get_company()`](https://statzhero.github.io/wrds/reference/get_company.md)
  for company header data

- [`link_ccm()`](https://statzhero.github.io/wrds/reference/link_ccm.md)
  for CRSP-Compustat linking

The lazy table can be filtered, selected, and mutated using dplyr verbs,
which are translated to SQL and executed on the server:

    get_table(wrds, "crsp", "msf") |>
      filter(date >= "2025-01-01") |>
      select(permno, date, ret, prc) |>
      collect()

## See also

[`describe_table()`](https://statzhero.github.io/wrds/reference/describe_table.md)
to explore table structure,
[`list_tables()`](https://statzhero.github.io/wrds/reference/list_tables.md)
to list available tables in a library

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()

# Preview table structure first
describe_table(wrds, "crsp", "msf")

# Get a lazy table and build your query
get_table(wrds, "crsp", "msf") |>
  dplyr::filter(date >= "2025-01-01") |>
  dplyr::select(permno, date, ret, prc, vol) |>
  dplyr::collect()

# Collect immediately with specific columns
get_table(wrds, "crsp", "dsf",
  columns = c("permno", "date", "ret", "prc"),
  lazy = FALSE,
  n = 1000
)

# Access any table in any library
get_table(wrds, "ibes", "statsum_epsus") |>
  dplyr::filter(fpedats >= "2025-01-01") |>
  dplyr::collect()

wrds_disconnect(wrds)
} # }
```
