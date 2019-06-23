test_that("productivity levels are aligned with the expected output", {
  # numeric output ----
  pl_n <- productivity_levels(
    trade_data = services_trade_2016$services_trade_2016_matrix,
    country1 = "country",
    product1 = "product",
    value1 = "value",
    gdp_data = gdp_pc_2016$gdp_pc_2016_numeric,
    country2 = "country",
    value2 = "value"
  )

  expect_is(pl_n, "list")
  expect_is(pl_n$economies_productivity_level, "numeric")
  expect_is(pl_n$products_productivity_level, "numeric")
  expect_equal(length(pl_n$economies_productivity_level), 80)
  expect_equal(length(pl_n$products_productivity_level), 11)

  pl_n_2 <- productivity_levels(
    trade_data = services_trade_2016$services_trade_2016_tibble,
    country1 = "country",
    product1 = "product",
    value1 = "value",
    gdp_data = gdp_pc_2016$gdp_pc_2016_tibble,
    country2 = "country",
    value2 = "value"
  )

  expect_is(pl_n_2, "list")
  expect_is(pl_n_2$economies_productivity_level, "numeric")
  expect_is(pl_n_2$products_productivity_level, "numeric")
  expect_equal(length(pl_n_2$economies_productivity_level), 80)
  expect_equal(length(pl_n_2$products_productivity_level), 11)

  expect_equal(pl_n, pl_n_2)

  # tibble output ----
  pl_t <- productivity_levels(
    trade_data = services_trade_2016$services_trade_2016_matrix,
    country1 = "country",
    product1 = "product",
    value1 = "value",
    gdp_data = gdp_pc_2016$gdp_pc_2016_numeric,
    country2 = "country",
    value2 = "value",
    tbl_output = T
  )

  expect_is(pl_t, "list")
  expect_is(pl_t$economies_productivity_level, "data.frame")
  expect_is(pl_t$products_productivity_level, "data.frame")
  expect_equal(nrow(pl_t$economies_productivity_level), 80)
  expect_equal(ncol(pl_t$economies_productivity_level), 2)
  expect_equal(nrow(pl_t$products_productivity_level), 11)
  expect_equal(ncol(pl_t$products_productivity_level), 2)

  pl_t_2 <- productivity_levels(
    trade_data = services_trade_2016$services_trade_2016_matrix,
    country1 = "country",
    product1 = "product",
    value1 = "value",
    gdp_data = gdp_pc_2016$gdp_pc_2016_numeric,
    country2 = "country",
    value2 = "value",
    tbl_output = T
  )

  expect_is(pl_t_2, "list")
  expect_is(pl_t_2$economies_productivity_level, "data.frame")
  expect_is(pl_t_2$products_productivity_level, "data.frame")
  expect_equal(nrow(pl_t_2$economies_productivity_level), 80)
  expect_equal(ncol(pl_t_2$economies_productivity_level), 2)
  expect_equal(nrow(pl_t_2$products_productivity_level), 11)
  expect_equal(ncol(pl_t_2$products_productivity_level), 2)

  expect_equal(pl_t, pl_t_2)
})
