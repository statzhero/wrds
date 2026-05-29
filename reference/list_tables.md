# List tables in a library

Returns a tibble of table names within a WRDS library (schema), with
human-readable descriptions where available.

## Usage

``` r
list_tables(wrds, library)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

- library:

  Character. The name of the library (schema) to query.

## Value

A tibble with columns `table` and `description`.

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
list_tables(wrds, "comp")
wrds_disconnect(wrds)
} # }
```
