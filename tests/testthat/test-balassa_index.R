test_that("balassa_index works with a sparse matrix", {
  bi <- balassa_index(world_trade_avg_1998_to_2000)

  expect_is(bi, "dgCMatrix")
  expect_equal(nrow(bi), 226)
  expect_equal(ncol(bi), 785)
  expect_equal(min(bi), 0)
  expect_equal(max(bi), 1)
})

test_that("balassa_index works with a dense matrix", {
  wt <- Matrix::as.matrix(world_trade_avg_1998_to_2000)

  bi <- balassa_index(world_trade_avg_1998_to_2000)

  bi2 <- balassa_index(wt)

  expect_is(bi2, "dgCMatrix")
  expect_equal(bi, bi2)
  expect_equal(nrow(bi2), 226)
  expect_equal(ncol(bi2), 785)
  expect_equal(min(bi2), 0)
  expect_equal(max(bi2), 1)
})

test_that("balassa_index works with a data frame", {
  wt <- Matrix::as.matrix(world_trade_avg_1998_to_2000)
  wt <- as.data.frame(as.table(wt), stringsAsFactors = FALSE)

  bi <- balassa_index(world_trade_avg_1998_to_2000)

  bi2 <- balassa_index(
    data = wt, country = "Var1", product = "Var2", value = "Freq"
  )

  expect_is(bi2, "dgCMatrix")
  expect_equal(bi, bi2)
  expect_equal(nrow(bi2), 226)
  expect_equal(ncol(bi2), 785)
  expect_equal(min(bi2), 0)
  expect_equal(max(bi2), 1)
})

test_that("balassa_index returns error with vector data", {
  expect_error(
    balassa_index(
      data = 200:100,
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
