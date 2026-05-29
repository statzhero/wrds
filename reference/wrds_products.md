# WRDS product catalog

A mapping of WRDS schema names to human-readable product names. Used by
[`list_subscriptions()`](https://statzhero.github.io/wrds/reference/list_subscriptions.md)
to enrich results, and available for users who want to look up product
names directly.

## Usage

``` r
wrds_products
```

## Format

A data frame with 613 rows and 2 columns:

- schema:

  WRDS schema or product code

- product:

  Human-readable product name

## Source

<https://wrds-www.wharton.upenn.edu/users/products/>
