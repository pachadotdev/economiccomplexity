test_that("proximity results are aligned with the expected output ", {
  # matrix output ----
  pr_m <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_matrix,
    diversity = package_output_demo$complexity_measures_numeric$diversity,
    ubiquity = package_output_demo$complexity_measures_numeric$ubiquity
  )

  expect_is(pr_m, "list")
  expect_equal(nrow(pr_m$country_proximity), 80)
  expect_equal(ncol(pr_m$country_proximity), 80)
  expect_equal(nrow(pr_m$product_proximity), 11)
  expect_equal(ncol(pr_m$product_proximity), 11)
  expect_gte(min(pr_m$country_proximity), 0)
  expect_lte(max(pr_m$country_proximity), 1)
  expect_gte(min(pr_m$product_proximity), 0)
  expect_lte(max(pr_m$product_proximity), 1)

  pr_m_2 <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_tibble,
    diversity = package_output_demo$complexity_measures_tibble$diversity,
    ubiquity = package_output_demo$complexity_measures_tibble$ubiquity
  )

  expect_is(pr_m, "list")
  expect_equal(nrow(pr_m$country_proximity), 80)
  expect_equal(ncol(pr_m$country_proximity), 80)
  expect_equal(nrow(pr_m$product_proximity), 11)
  expect_equal(ncol(pr_m$product_proximity), 11)
  expect_gte(min(pr_m$country_proximity), 0)
  expect_lte(max(pr_m$country_proximity), 1)
  expect_gte(min(pr_m$product_proximity), 0)
  expect_lte(max(pr_m$product_proximity), 1)

  # tibble output ----
  pr_t <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_matrix,
    diversity = package_output_demo$complexity_measures_numeric$diversity,
    ubiquity = package_output_demo$complexity_measures_numeric$ubiquity,
    tbl_output = T
  )

  expect_is(pr_t, "list")
  expect_equal(nrow(pr_t$country_proximity), 2683)
  expect_equal(ncol(pr_t$country_proximity), 3)
  expect_equal(nrow(pr_t$product_proximity), 51)
  expect_equal(ncol(pr_t$product_proximity), 3)
  expect_gte(min(pr_t$country_proximity$value), 0)
  expect_lte(max(pr_t$country_proximity$value), 1)
  expect_gte(min(pr_t$product_proximity$value), 0)
  expect_lte(max(pr_t$product_proximity$value), 1)

  pr_t_2 <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_tibble,
    diversity = package_output_demo$complexity_measures_tibble$diversity,
    ubiquity = package_output_demo$complexity_measures_tibble$ubiquity,
    tbl_output = T
  )

  expect_is(pr_t, "list")
  expect_equal(nrow(pr_t$country_proximity), 2683)
  expect_equal(ncol(pr_t$country_proximity), 3)
  expect_equal(nrow(pr_t$product_proximity), 51)
  expect_equal(ncol(pr_t$product_proximity), 3)
  expect_gte(min(pr_t$country_proximity$value), 0)
  expect_lte(max(pr_t$country_proximity$value), 1)
  expect_gte(min(pr_t$product_proximity$value), 0)
  expect_lte(max(pr_t$product_proximity$value), 1)
})
