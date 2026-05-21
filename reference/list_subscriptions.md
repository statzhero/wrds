# List subscribed data products

Returns a character vector of WRDS schemas the user has access to.

## Usage

``` r
list_subscriptions(wrds)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

## Value

A character vector of schema names.

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
list_subscriptions(wrds)
wrds_disconnect(wrds)
} # }
```
