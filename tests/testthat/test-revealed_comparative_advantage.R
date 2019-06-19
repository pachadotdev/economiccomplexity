test_that("rca matrix/tibble fulfills desired properties", {
  # matrix output ----
  rca_m <- revealed_comparative_advantage(
    data = services_trade_2016_matrix, country = "country", product = "product", value = "value"
  )
  expect_is(rca_m, "Matrix")
  expect_equal(nrow(rca_m), 80)
  expect_equal(ncol(rca_m), 11)
  expect_equal(min(rca_m), 0)
  expect_equal(max(rca_m), 1)

  rca_m <- revealed_comparative_advantage(
    data = services_trade_2016_tibble, country = "country", product = "product", value = "value"
  )
  expect_is(rca_m, "Matrix")
  expect_equal(nrow(rca_m), 80)
  expect_equal(ncol(rca_m), 11)
  expect_equal(min(rca_m), 0)
  expect_equal(max(rca_m), 1)

  # tibble output ----
  rca_t <- revealed_comparative_advantage(
    data = services_trade_2016_matrix, country = "country", product = "product", value = "value",
    tbl_output = T
  )
  expect_is(rca_t, "data.frame")
  expect_equal(nrow(rca_t), 735)
  expect_equal(ncol(rca_t), 3)
  expect_equal(min(rca_t$value), 0)
  expect_equal(max(rca_t$value), 1)

  rca_t <- revealed_comparative_advantage(
    data = services_trade_2016_tibble, country = "country", product = "product", value = "value",
    tbl_output = T
  )
  expect_is(rca_t, "data.frame")
  expect_equal(nrow(rca_t), 735)
  expect_equal(ncol(rca_t), 3)
  expect_equal(min(rca_t$value), 0)
  expect_equal(max(rca_t$value), 1)
})
