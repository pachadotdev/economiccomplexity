test_that("balassa_index works with a data frame", {
  bi <- balassa_index(
    data = galactic_federation, source = "planet", target = "product", value = "export_value"
  )

  expect_is(bi, "dgCMatrix")
  expect_equal(nrow(bi), 9)
  expect_equal(ncol(bi), 12)
  expect_equal(min(bi), 0)
  expect_equal(max(bi), 1)
})

test_that("balassa_index returns error with vector data", {
  expect_error(
    balassa_index(
      data = as.numeric(galactic_federation$export_value),
      source = "country",
      target = "product",
      value = "export_value"
    )
  )
})

test_that("balassa_index returns error with numeric source/target", {
  expect_error(
    balassa_index(
      data = galactic_federation,
      source = 200,
      target = 100,
      value = "export_value"
    )
  )
})

test_that("balassa_index returns error with character discrete", {
  expect_error(
    balassa_index(
      data = galactic_federation,
      source = "country",
      target = "product",
      value = "export_value",
      discrete = "yes"
    )
  )
})

test_that("balassa_index returns error with character cutoff", {
  expect_error(
    balassa_index(
      data = galactic_federation,
      source = "country",
      target = "product",
      value = "export_value",
      cutoff = "one"
    )
  )
})
