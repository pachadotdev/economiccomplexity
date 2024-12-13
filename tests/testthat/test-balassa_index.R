test_that("balassa_index works with a data.frame", {
  bi <- expect_warning(rca())
  bi <- balassa_index(world_trade_avg_1998_to_2000)

  expect_is(bi, "matrix")
  expect_equal(nrow(bi), 226)
  expect_equal(ncol(bi), 785)
  expect_equal(min(bi), 0)
  expect_equal(max(bi), 1)
})

test_that("balassa_index works with a matrix", {
  wt <- world_trade_avg_1998_to_2000
  wt$country <- as.factor(wt$country)
  wt$product <- as.factor(wt$product)

  wt <- dataframe_to_matrix(wt, country = "country", product = "product",
    value = "value")

  bi <- balassa_index(world_trade_avg_1998_to_2000)

  bi2 <- balassa_index(wt)

  expect_is(bi2, "matrix")
  expect_equal(bi, bi2)
  expect_equal(nrow(bi2), 226)
  expect_equal(ncol(bi2), 785)
  expect_equal(min(bi2), 0)
  expect_equal(max(bi2), 1)
})

test_that("balassa_index returns error with vector data", {
  expect_error(
    balassa_index(
      data = seq(200, 100, 1),
      country = "country",
      product = "product",
      value = "export_value"
    )
  )
})

test_that("balassa_index returns error with numeric country/product", {
  expect_error(
    balassa_index(
      data = world_trade_avg_1998_to_2000,
      country = 200,
      product = 100,
      value = "export_value"
    )
  )
})

test_that("balassa_index returns error with character discrete", {
  expect_error(
    balassa_index(
      data = world_trade_avg_1998_to_2000,
      country = "country",
      product = "product",
      value = "export_value",
      discrete = "yes"
    )
  )
})

test_that("balassa_index returns error with character cutoff", {
  expect_error(
    balassa_index(
      data = world_trade_avg_1998_to_2000,
      country = "country",
      product = "product",
      value = "export_value",
      cutoff = "one"
    )
  )
})
