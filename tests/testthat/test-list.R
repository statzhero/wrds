# Integration tests

test_that("list_subscriptions returns tibble with schema and product", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  subs <- list_subscriptions(wrds)
  expect_s3_class(subs, "tbl_df")
  expect_named(subs, c("schema", "product"))
  expect_true(nrow(subs) > 0)

  expect_true("comp" %in% subs$schema)
  expect_true("crsp" %in% subs$schema)

  # Friendly schemas should have product names from the lookup
  comp_product <- subs$product[subs$schema == "comp"]
  expect_equal(comp_product, "Compustat")
})

test_that("list_tables returns tibble with table and description", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  tables <- list_tables(wrds, "comp")
  expect_s3_class(tables, "tbl_df")
  expect_named(tables, c("table", "description"))
  expect_true("funda" %in% tables$table)
  expect_true("fundq" %in% tables$table)
})

test_that("describe_table returns metadata with labels", {
  skip_on_cran()
  skip_if_no_wrds()

  wrds <- wrds_connect()
  withr::defer(wrds_disconnect(wrds))

  desc <- describe_table(wrds, "comp", "funda")
  expect_type(desc, "list")

  expect_named(desc, c("columns", "description", "nrow", "sample"))
  expect_s3_class(desc$columns, "data.frame")
  expect_true("gvkey" %in% desc$columns$column_name)
  expect_true("datadate" %in% desc$columns$column_name)
  expect_true("label" %in% names(desc$columns))
  expect_s3_class(desc$sample, "data.frame")
  expect_equal(nrow(desc$sample), 20)
})
