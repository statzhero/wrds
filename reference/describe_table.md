# Describe a table

Displays a glimpse-like summary of a WRDS table showing column names,
types, and human-readable labels, similar to
[`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html).

## Usage

``` r
describe_table(wrds, library, table, n = 20, max_cols = 25)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

- library:

  Character. The name of the library (schema).

- table:

  Character. The name of the table.

- n:

  Integer. Number of sample rows to fetch for value preview. Default is
  20.

- max_cols:

  Integer. Maximum number of columns to display. Default is 25.

## Value

Invisibly returns a list with components:

- columns:

  A data frame with `column_name`, `data_type`, and `label`

- description:

  Table description, or `NA` if unavailable

- nrow:

  Row count

- sample:

  A data frame with sample rows (if `n > 0`)

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
describe_table(wrds, "comp", "funda")
wrds_disconnect(wrds)
} # }
```
