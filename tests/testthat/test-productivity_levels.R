test_that("productivity levels are aligned with the expected output", {
  pl <- ec_productivity_levels(
    data = ec_trade_1962,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    gdp = ec_gdp_pc_1962,
    c2 = "country",
    v2 = "value",
    tbl = T
  )

  expect_is(pl, "list")
  expect_is(pl$productivity_level_country, "data.frame")
  expect_is(pl$productivity_level_product, "data.frame")
  expect_equal(nrow(pl$productivity_level_country), 89)
  expect_equal(ncol(pl$productivity_level_country), 2)
  expect_equal(nrow(pl$productivity_level_product), 991)
  expect_equal(ncol(pl$productivity_level_product), 2)
})
