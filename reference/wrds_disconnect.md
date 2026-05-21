# Disconnect from WRDS

Closes a WRDS database connection.

## Usage

``` r
wrds_disconnect(wrds)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

## Value

Invisibly returns `TRUE` if disconnection was successful.

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
wrds_disconnect(wrds)
} # }
```
