test_that("productivity levels are aligned with the expected output", {
  # numeric output ----
  # pl_n <- productivity_levels(
  #   trade_data = services_trade_2016_matrix, c1 = "country", p1 = "product", v1 = "value",
  #   gdp_data = gdp_pc_2016_numeric, c2 = "country", v2 = "value",
  #   tbl_output = F
  # )
  # expect_is(pl_n, "list")
  # expect_is(pl_n$economies_productivity_level, "numeric")
  # expect_is(pl_n$products_productivity_level, "numeric")
  # expect_equal(length(pl_n$economies_productivity_level), 9)
  # expect_equal(length(pl_n$products_productivity_level), 12)

  # tibble output ----
  pl_t <- productivity_levels(
    trade_data = services_trade_2016_tibble, c1 = "country", p1 = "product", v1 = "value",
    gdp_data = gdp_pc_2016_tibble, c2 = "country", v2 = "value",
    tbl_output = T
  )
  expect_is(pl_t, "list")
  expect_is(pl_t$economies_productivity_level, "data.frame")
  expect_is(pl_t$products_productivity_level, "data.frame")
  expect_equal(nrow(pl_t$economies_productivity_level), 80)
  expect_equal(ncol(pl_t$economies_productivity_level), 2)
  expect_equal(nrow(pl_t$products_productivity_level), 11)
  expect_equal(ncol(pl_t$products_productivity_level), 2)
})
