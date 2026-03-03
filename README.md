# wrds
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/wrds)](https://CRAN.R-project.org/package=wrds)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Simple functions for accessing data from [Wharton Research Data Services](https://wrds-www.wharton.upenn.edu/) (WRDS). A WRDS account is required.

## Installation

Install from CRAN:

```r
install.packages("wrds")
```

Or install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("statzhero/wrds")
```

## Setup

Before first use, store your WRDS credentials securely:

```r
library(wrds)
wrds_set_credentials()
```

Credentials are stored in your system's keyring (Keychain on macOS, Credential Manager on Windows).

## Usage

### Explore

```r
library(wrds)

wrds <- wrds_connect()

# Discover available data
list_libraries(wrds)
list_tables(wrds, "comp")

# Inspect table structure
describe_table(wrds, "comp", "funda", max_cols = 5)
#> comp.funda
#> Rows: 929,001
#> Columns: 948
#> $ gvkey    <chr>  "001000", "001000", "001000", "001000", "001000", ...
#> $ datadate <date> 1961-12-31, 1962-12-31, 1963-12-31, 1964-12-31, ...
#> $ fyear    <int>  1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, ...
#> $ indfmt   <chr>  "INDL", "INDL", "INDL", "INDL", "INDL", "INDL", "INDL", ...
#> $ consol   <chr>  "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", ...
#> # ... with 943 more columns
```

To browse interactively in the [Positron](https://positron.posit.co/) or [RStudio](https://posit.co/products/open-source/rstudio/) Connections pane:

```r
library(connections)
connection_view(wrds)
```

### Download

Pre-built functions for common datasets:

```r
# Compustat annual fundamentals (North America)
funda <- get_compustat(wrds,
  start_date = "2024-01-01",
  end_date = "2025-12-31"
)

# Compustat quarterly (Global)
g_fundq <- get_compustat(wrds,
  frequency = "quarterly",
  region = "global",
  start_date = "2025-01-01"
)

# Company header data (SIC, NAICS, state, etc.)
company <- get_company(wrds)

# Linking tables
ccm <- link_ccm(wrds)
ibes_link <- link_ibes_crsp(wrds)

# Preview before downloading
preview <- get_compustat(wrds, n = 100)

# Historical SIC codes are included by default
get_compustat(wrds, n = 100) |> dplyr::count(sich)
#> # A tibble: 4 × 2
#>    sich     n
#>   <int> <int>
#> 1  4841     1
#> 2  5080    24
#> 3  5712     3
#> 4    NA    72

# Fill missing historical SIC codes with SIC from comp.company
get_compustat(wrds, fill_sic = TRUE, n = 100) |> dplyr::count(sic)
#> # A tibble: 8 × 2
#>   sic       n
#>   <chr> <int>
#> 1 3089     17
#> 2 3825     13
#> 3 3949      3
#> 4 4841      2
#> 5 4911      1
#> 6 5080     46
#> 7 5712     10
#> 8 5812      8

wrds_disconnect(wrds)
```

### Custom queries

Access any table and build queries with dplyr:

```r
# Filter CRSP monthly returns for specific stocks
big_tech <- c(14593, 90319, 13407)  # Apple, Alphabet, Meta
get_table(wrds, "crsp", "msf") |>
  filter(permno %in% big_tech, date >= "2020-01-01") |>
  select(permno, date, ret, prc, shrout) |>
  collect()

# Lazy queries for large datasets
get_compustat(wrds, lazy = TRUE) |>
  filter(fyear >= 2000) |>
  select(gvkey, datadate, at, sale) |>
  collect()
```

## Non-standard usage

`get_compustat()` applies standard research filters by default. You can customize these for specialized analyses:

```r
# Financial services firms (banks, insurance, etc.)
# Default indfmt = "INDL" excludes these
get_compustat(wrds, indfmt = "FS", n = 100)

# Include non-consolidated statements
# Default consol = "C" returns only consolidated
get_compustat(wrds, consol = "B", n = 100)

# Add columns beyond the defaults
get_compustat(wrds, add_columns = c("emp"), n = 100)

# Specify your own column list (replaces defaults)
get_compustat(wrds, columns = c("gvkey", "datadate", "at", "lt", "seq"), n = 100)

```

**Default filters by region:**

| Filter | North America | Global (without US) |
|--------|---------------|--------|
| `datafmt` | `"STD"` | `"HIST_STD"` |
| `popsrc` | `"D"` (domestic) | `"I"` (international) |
| `indfmt` | `"INDL"` | `"INDL"` |
| `consol` | `"C"` | `"C"` |

To inspect the default columns and table names for any frequency/region combination:

```r
wrds:::compustat_config("annual", "na")
wrds:::compustat_config("quarterly", "global")
```

## Functions

| Function | Description |
|----------|-------------|
| `wrds_connect()` | Connect to WRDS using keyring credentials |
| `wrds_disconnect()` | Close connection |
| `wrds_set_credentials()` | Store credentials in system keyring |
| `list_libraries()` | List available schemas |
| `list_tables()` | List tables in a schema |
| `describe_table()` | Glimpse table structure and sample values |
| `get_table()` | Generic access to any WRDS table |
| `get_compustat()` | Download Compustat fundamentals |
| `get_company()` | Download Compustat company header data |
| `link_ccm()` | Get CRSP-Compustat linking table |
| `link_ibes_crsp()` | Get IBES-CRSP linking table |

## References

- [WRDS](https://wrds-www.wharton.upenn.edu/)
- [Python wrds package](https://github.com/wharton/wrds)
- [Financial Accounting Research (FAR Book)](https://iangow.github.io/far_book/)

