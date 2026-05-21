# Smart collect with size awareness

Internal function that collects a lazy table with warnings for large
queries.

## Usage

``` r
smart_collect(tbl, wrds, lazy = FALSE)
```

## Arguments

- tbl:

  A `tbl_lazy` object from dbplyr.

- wrds:

  Database connection for row counting.

- lazy:

  If `TRUE`, return the lazy table without collecting.

## Value

A tibble if collecting, or the lazy table if `lazy = TRUE`.
