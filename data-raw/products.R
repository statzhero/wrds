# Maintainer-only script to rebuild data/wrds_products.rda
#
# To update:
# 1. Export CSV from https://wrds-www.wharton.upenn.edu/users/products/
# 2. Save as data-raw/products.csv
# 3. Run: source("data-raw/products.R")

library(readr)
library(dplyr)

products_raw <- read_csv("data-raw/products.csv", show_col_types = FALSE)

wrds_products <- products_raw |>
  mutate(
    schema = `Product code`,
    product = Description,
    .keep = "none"
  ) |>
  mutate(product = replace_values(product, "None" ~ NA))

# Friendly view schemas that users interact with most.
# These aren't product codes, so they're missing from the CSV.
friendly_schemas <- tibble::tribble(
  ~schema,
  ~product,
  "comp",
  "Compustat",
  "crsp",
  "CRSP",
  "ibes",
  "I/B/E/S",
  "optionm",
  "OptionMetrics",
  "taq",
  "NYSE TAQ",
)

wrds_products <- bind_rows(friendly_schemas, wrds_products)

usethis::use_data(wrds_products, overwrite = TRUE)
