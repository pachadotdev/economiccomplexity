test_that("productivity levels are aligned with the expected output", {
  # numeric output ----
  pl_n <- productivity_levels(
    trade_data = world_trade_2017, c1 = "reporter_iso", p1 = "product_code", v1 = "export_value_usd",
    gdp_data = world_gdp_and_population_2017, c2 = "reporter_iso", v2 = "gdp_pc_usd",
    tbl_output = F
  )
  expect_is(pl_n, "list")
  expect_is(pl_n$economies_productivity_level, "numeric")
  expect_is(pl_n$products_productivity_level, "numeric")
  expect_equal(length(pl_n$economies_productivity_level), 194)
  expect_equal(length(pl_n$products_productivity_level), 1222)

  # tibble output ----
  pl_t <- productivity_levels(
    trade_data = world_trade_2017, c1 = "reporter_iso", p1 = "product_code", v1 = "export_value_usd",
    gdp_data = world_gdp_and_population_2017, c2 = "reporter_iso", v2 = "gdp_pc_usd",
    tbl_output = T
  )
  expect_is(pl_t, "list")
  expect_is(pl_t$economies_productivity_level, "data.frame")
  expect_is(pl_t$products_productivity_level, "data.frame")
  expect_equal(nrow(pl_t$economies_productivity_level), 194)
  expect_equal(ncol(pl_t$economies_productivity_level), 2)
  expect_equal(nrow(pl_t$products_productivity_level), 1222)
  expect_equal(ncol(pl_t$products_productivity_level), 2)
})
