test_that("proximity results are aligned with the expected output ", {
  # matrix output ----
  pr_m <- proximity(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_matrix,
    diversity = package_output_demo$complexity_measures_numeric$diversity,
    ubiquity = package_output_demo$complexity_measures_numeric$ubiquity
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

  pr_m_2 <- proximity(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_tibble,
    diversity = package_output_demo$complexity_measures_tibble$diversity,
    ubiquity = package_output_demo$complexity_measures_tibble$ubiquity
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

  expect_equal(pr_m$countries_proximity, pr_m_2$countries_proximity)
  expect_equal(pr_m$products_proximity, pr_m_2$products_proximity)

  # tibble output ----
  pr_t <- proximity(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_matrix,
    diversity = package_output_demo$complexity_measures_numeric$diversity,
    ubiquity = package_output_demo$complexity_measures_numeric$ubiquity,
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

  pr_t_2 <- proximity(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_tibble,
    diversity = package_output_demo$complexity_measures_tibble$diversity,
    ubiquity = package_output_demo$complexity_measures_tibble$ubiquity,
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

  expect_equal(pr_t$countries_proximity, pr_t_2$countries_proximity)
  expect_equal(pr_t$products_proximity, pr_t_2$products_proximity)
})
