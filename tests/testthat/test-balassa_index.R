test_that("balassa_index works with a sparse matrix", {
  bi <- balassa_index(
    data = galactic_federation, country = "planet", product = "product", value = "export_value"
  )

  expect_is(bi, "dgCMatrix")
  expect_equal(nrow(bi), 9)
  expect_equal(ncol(bi), 12)
  expect_equal(min(bi), 0)
  expect_equal(max(bi), 1)
})

test_that("balassa_index works with a dense matrix", {
  gf <- Matrix::as.matrix(galactic_federation)

  bi <- balassa_index(data = galactic_federation)

  bi2 <- balassa_index(data = gf)

  expect_is(bi2, "dgCMatrix")
  expect_equal(bi, bi2)
  expect_equal(nrow(bi2), 9)
  expect_equal(ncol(bi2), 12)
  expect_equal(min(bi2), 0)
  expect_equal(max(bi2), 1)
})

test_that("balassa_index works with a data frame", {
  gf <- Matrix::as.matrix(galactic_federation)
  gf <- as.data.frame(as.table(gf), stringsAsFactors = FALSE)

  bi <- balassa_index(
    data = galactic_federation, country = "planet", product = "product", value = "export_value"
  )

  bi2 <- balassa_index(
    data = gf, country = "Var1", product = "Var2", value = "Freq"
  )

  expect_is(bi2, "dgCMatrix")
  expect_equal(bi, bi2)
  expect_equal(nrow(bi2), 9)
  expect_equal(ncol(bi2), 12)
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
      data = galactic_federation,
      country = 200,
      product = 100,
      value = "export_value"
    )
  )
})

test_that("balassa_index returns error with character discrete", {
  expect_error(
    balassa_index(
      data = galactic_federation,
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
      data = galactic_federation,
      country = "country",
      product = "product",
      value = "export_value",
      cutoff = "one"
    )
  )
})
