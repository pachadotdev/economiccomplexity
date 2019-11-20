test_that("rca output is aligned with the expected output ", {
  r <- rca(ec_trade_1962)
  expect_is(r, "data.frame")
  expect_equal(nrow(r), 44435)
  expect_equal(min(r$value), 0)
  expect_equal(max(r$value), 1)
})
