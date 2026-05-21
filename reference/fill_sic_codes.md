# Fill missing SIC codes from company header

Internal function that joins fundamentals data with company header to
fill missing historical SIC codes using coalesce.

## Usage

``` r
fill_sic_codes(tbl, wrds)
```

## Arguments

- tbl:

  A lazy table from funda/fundq.

- wrds:

  Database connection.

## Value

A collected tibble with `sic` column (character, coalesced from `sich`
and header `sic`).
