test_that("proximity results are aligned with the expected output", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
    balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_country), 9)
  expect_equal(nrow(pr$proximity_product), 12)
  expect_gte(min(pr$proximity_country), 0)
  expect_lte(max(pr$proximity_country), 1)
  expect_gte(min(pr$proximity_product), 0)
  expect_lte(max(pr$proximity_product), 1)
})

test_that("proximity returns country proximity only", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
    balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product,
    compute = "country"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_country), 9)
  expect_equal(nrow(pr$proximity_product), NULL)
  expect_gte(min(pr$proximity_country), 0)
  expect_lte(max(pr$proximity_country), 1)
})

test_that("proximity returns product proximity only", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
    balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product,
    compute = "product"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_country), NULL)
  expect_equal(nrow(pr$proximity_product), 12)
  expect_gte(min(pr$proximity_product), 0)
  expect_lte(max(pr$proximity_product), 1)
})

test_that("proximity fails with NULL balassa_index", {
  expect_error(
    proximity(
      balassa_index = NULL,
      balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
      balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product
    )
  )
})

test_that("proximity fails with NULL names in balassa_sum_country/balassa_sum_product", {
  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_country = as.numeric(economiccomplexity_output$complexity_measures$balassa_sum_country$value),
      balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product
    )
  )

  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
      balassa_sum_product = as.numeric(economiccomplexity_output$complexity_measures$balassa_sum_product$value)
    )
  )
})

test_that("proximity fails with NULL balassa_sum_country/balassa_sum_product", {
  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_country = NULL,
      balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product
    )
  )

  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
      balassa_sum_product = NULL
    )
  )
})

test_that("proximity fails with NULL compute", {
  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
      balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product,
      compute = NULL
    )
  )
})
