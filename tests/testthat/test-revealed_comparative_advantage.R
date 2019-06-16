test_that("rca matrix/tibble fulfills desired properties", {
  # matrix output ----
  rca_m <- revealed_comparative_advantage(
    data = world_trade_2017, c = "reporter_iso", p = "product_code", v = "export_value_usd"
  )
  expect_is(rca_m, "Matrix")
  expect_equal(nrow(rca_m), 224)
  expect_equal(ncol(rca_m), 1222)
  expect_equal(min(rca_m), 0)
  expect_equal(max(rca_m), 1)

  # tibble output ----
  rca_t <- revealed_comparative_advantage(
    data = world_trade_2017, c = "reporter_iso", p = "product_code", v = "export_value_usd",
    tbl_output = T
  )
  expect_is(rca_t, "data.frame")
  expect_equal(nrow(rca_t), 176971)
  expect_equal(ncol(rca_t), 3)
  expect_equal(min(rca_t$value), 0)
  expect_equal(max(rca_t$value), 1)
})
