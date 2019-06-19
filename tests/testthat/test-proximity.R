test_that("proximity results are aligned with the expected output ", {
  # matrix output ----
  pr_m <- proximity(
    rca = rca_m,
    diversity = cm_n$diversity,
    ubiquity = cm_n$ubiquity
  )
  expect_is(pr_m, "list")
  expect_equal(nrow(pr_m$countries_proximity), 80)
  expect_equal(ncol(pr_m$countries_proximity), 80)
  expect_equal(nrow(pr_m$products_proximity), 11)
  expect_equal(ncol(pr_m$products_proximity), 11)
  expect_gte(min(pr_m$countries_proximity), 0)
  expect_lte(max(pr_m$countries_proximity), 1)
  expect_gte(min(pr_m$products_proximity), 0)
  expect_lte(max(pr_m$products_proximity), 1)

  # tibble output ----
  pr_t <- proximity(
    rca = rca_t,
    diversity = cm_t$diversity,
    ubiquity = cm_t$ubiquity,
    tbl_output = T
  )
  expect_is(pr_t, "list")
  expect_equal(nrow(pr_t$countries_proximity), 2683)
  expect_equal(ncol(pr_t$countries_proximity), 3)
  expect_equal(nrow(pr_t$products_proximity), 51)
  expect_equal(ncol(pr_t$products_proximity), 3)
  expect_gte(min(pr_t$countries_proximity$value), 0)
  expect_lte(max(pr_t$countries_proximity$value), 1)
  expect_gte(min(pr_t$products_proximity$value), 0)
  expect_lte(max(pr_t$products_proximity$value), 1)
})
