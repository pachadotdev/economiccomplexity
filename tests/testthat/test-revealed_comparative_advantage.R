test_that("rca matrix/tibble fulfills desired properties", {
  rca <- ec_rca(
    data = ec_trade_1962,
    tbl = T
  )

  expect_is(rca, "data.frame")
  expect_equal(nrow(rca), 44435)
  expect_equal(min(rca$value), 0)
  expect_equal(max(rca$value), 1)
})
