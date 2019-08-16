test_that("proximity results are aligned with the expected output ", {
  # matrix output ----
  pr_m <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_matrix,
    diversity = package_output_demo$complexity_measures_numeric$diversity,
    ubiquity = package_output_demo$complexity_measures_numeric$ubiquity
  )

  expect_is(pr_m, "list")
  expect_equal(nrow(pr_m$proximity_countries), 80)
  expect_equal(ncol(pr_m$proximity_countries), 80)
  expect_equal(nrow(pr_m$proximity_products), 11)
  expect_equal(ncol(pr_m$proximity_products), 11)
  expect_gte(min(pr_m$proximity_countries), 0)
  expect_lte(max(pr_m$proximity_countries), 1)
  expect_gte(min(pr_m$proximity_products), 0)
  expect_lte(max(pr_m$proximity_products), 1)

  pr_m_2 <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_tibble,
    diversity = package_output_demo$complexity_measures_tibble$diversity,
    ubiquity = package_output_demo$complexity_measures_tibble$ubiquity
  )

  expect_is(pr_m, "list")
  expect_equal(nrow(pr_m$proximity_countries), 80)
  expect_equal(ncol(pr_m$proximity_countries), 80)
  expect_equal(nrow(pr_m$proximity_products), 11)
  expect_equal(ncol(pr_m$proximity_products), 11)
  expect_gte(min(pr_m$proximity_countries), 0)
  expect_lte(max(pr_m$proximity_countries), 1)
  expect_gte(min(pr_m$proximity_products), 0)
  expect_lte(max(pr_m$proximity_products), 1)

  # expect_equivalent(pr_m$proximity_countries, pr_m_2$proximity_countries)
  # expect_equivalent(pr_m$proximity_products, pr_m_2$proximity_products)

  # tibble output ----
  pr_t <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_matrix,
    diversity = package_output_demo$complexity_measures_numeric$diversity,
    ubiquity = package_output_demo$complexity_measures_numeric$ubiquity,
    tbl_output = T
  )

  expect_is(pr_t, "list")
  expect_equal(nrow(pr_t$proximity_countries), 2683)
  expect_equal(ncol(pr_t$proximity_countries), 3)
  expect_equal(nrow(pr_t$proximity_products), 51)
  expect_equal(ncol(pr_t$proximity_products), 3)
  expect_gte(min(pr_t$proximity_countries$value), 0)
  expect_lte(max(pr_t$proximity_countries$value), 1)
  expect_gte(min(pr_t$proximity_products$value), 0)
  expect_lte(max(pr_t$proximity_products$value), 1)

  pr_t_2 <- proximity(
    rca = package_output_demo$revealed_comparative_advantage_tibble,
    diversity = package_output_demo$complexity_measures_tibble$diversity,
    ubiquity = package_output_demo$complexity_measures_tibble$ubiquity,
    tbl_output = T
  )

  expect_is(pr_t, "list")
  expect_equal(nrow(pr_t$proximity_countries), 2683)
  expect_equal(ncol(pr_t$proximity_countries), 3)
  expect_equal(nrow(pr_t$proximity_products), 51)
  expect_equal(ncol(pr_t$proximity_products), 3)
  expect_gte(min(pr_t$proximity_countries$value), 0)
  expect_lte(max(pr_t$proximity_countries$value), 1)
  expect_gte(min(pr_t$proximity_products$value), 0)
  expect_lte(max(pr_t$proximity_products$value), 1)

  # expect_equivalent(pr_t$proximity_countries, pr_t_2$proximity_countries)
  # expect_equivalent(pr_t$proximity_products, pr_t_2$proximity_products)
})
