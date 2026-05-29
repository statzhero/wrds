#' WRDS product catalog
#'
#' A mapping of WRDS schema names to human-readable product names.
#' Used by [list_subscriptions()] to enrich results, and available for
#' users who want to look up product names directly.
#'
#' @format A data frame with 613 rows and 2 columns:
#' \describe{
#'   \item{schema}{WRDS schema or product code}
#'   \item{product}{Human-readable product name}
#' }
#' @source <https://wrds-www.wharton.upenn.edu/users/products/>
"wrds_products"
