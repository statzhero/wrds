# Convert SIC codes to 2-digit industry codes

Extracts the first two characters from SIC codes to create broader
industry classifications.

## Usage

``` r
sic_2digit(sic)
```

## Arguments

- sic:

  A numeric or character vector of SIC codes.

## Value

A character vector of 2-digit SIC codes.

## Details

SIC codes are hierarchical: the first two digits represent major
industry groups (e.g., "54" = Retail-Food Stores), while the full
4-digit code provides more specific classifications (e.g., "5412" =
Retail-Convenience Stores).

## Examples

``` r
# Convenience Stores (SIC 5412) -> Retail-Food Stores (54)
sic_2digit(5412)
#> [1] "54"
# [1] "54"

sic_2digit(c(5412, 5400))
#> [1] "54" "54"
# [1] "54" "54"
```
