# Get Compustat configuration

Internal function returning table names and filters for Compustat
queries.

## Usage

``` r
compustat_config(frequency, region)
```

## Arguments

- frequency:

  One of `"annual"` or `"quarterly"`.

- region:

  One of `"na"` or `"global"`.

## Value

A list with `table`, `datafmt`, `popsrc`, and `columns`.
