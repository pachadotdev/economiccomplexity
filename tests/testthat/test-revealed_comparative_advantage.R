test_that("rca matrix/tibble fulfills desired properties", {
  # matrix output ----
  rca_m <- revealed_comparative_advantage(
    trade_data = services_trade_2016$services_trade_2016_matrix
  )

  expect_is(rca_m, "Matrix")
  expect_equal(nrow(rca_m), 80)
  expect_equal(ncol(rca_m), 11)
  expect_equal(min(rca_m), 0)
  expect_equal(max(rca_m), 1)

  rca_m_2 <- revealed_comparative_advantage(
    trade_data = services_trade_2016$services_trade_2016_tibble
  )

  expect_is(rca_m_2, "Matrix")
  expect_equal(nrow(rca_m_2), 80)
  expect_equal(ncol(rca_m_2), 11)
  expect_equal(min(rca_m_2), 0)
  expect_equal(max(rca_m_2), 1)

  expect_equal(rca_m, rca_m_2)

  # tibble output ----
  rca_t <- revealed_comparative_advantage(
    trade_data = services_trade_2016$services_trade_2016_matrix,
    tbl_output = T
  )

  expect_is(rca_t, "data.frame")
  expect_equal(nrow(rca_t), 735)
  expect_equal(ncol(rca_t), 3)
  expect_equal(min(rca_t$value), 0)
  expect_equal(max(rca_t$value), 1)

  rca_t_2 <- revealed_comparative_advantage(
    trade_data = services_trade_2016$services_trade_2016_tibble,
    tbl_output = T
  )

  expect_is(rca_t_2, "data.frame")
  expect_equal(nrow(rca_t_2), 735)
  expect_equal(ncol(rca_t_2), 3)
  expect_equal(min(rca_t_2$value), 0)
  expect_equal(max(rca_t_2$value), 1)

  expect_equal(rca_t, rca_t_2)
})
