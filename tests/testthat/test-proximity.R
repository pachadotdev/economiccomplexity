test_that("proximity results are aligned with the expected output", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_country), 226)
  expect_equal(nrow(pr$proximity_product), 785)
  expect_gte(min(pr$proximity_country), 0)
  expect_lte(max(pr$proximity_country), 1)
  expect_gte(min(pr$proximity_product), 0)
  expect_lte(max(pr$proximity_product), 1)
})

test_that("proximity returns country proximity only", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    compute = "country"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_country), 226)
  expect_equal(nrow(pr$proximity_product), NULL)
  expect_gte(min(pr$proximity_country), 0)
  expect_lte(max(pr$proximity_country), 1)
})

test_that("proximity returns product proximity only", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    compute = "product"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_country), NULL)
  expect_equal(nrow(pr$proximity_product), 785)
  expect_gte(min(pr$proximity_product), 0)
  expect_lte(max(pr$proximity_product), 1)
})

test_that("proximity fails with NULL balassa_index", {
  expect_error(
    proximity(
      balassa_index = NULL
    )
  )
})

test_that("proximity fails with NULL compute", {
  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      compute = NULL
    )
  )
})
