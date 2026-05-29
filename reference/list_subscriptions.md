# List subscribed data products

Returns a tibble of WRDS schemas the user has access to, with
human-readable product names where available.

## Usage

``` r
list_subscriptions(wrds)
```

## Arguments

- wrds:

  A `DBIConnection` object returned by
  [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md).

## Value

A tibble with columns `schema` and `product`.

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
list_subscriptions(wrds)
wrds_disconnect(wrds)
} # }
```
