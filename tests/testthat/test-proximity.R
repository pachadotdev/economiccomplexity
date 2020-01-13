test_that("proximity results are aligned with the expected output", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
    balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_source), 9)
  expect_equal(nrow(pr$proximity_target), 12)
  expect_gte(min(pr$proximity_source), 0)
  expect_lte(max(pr$proximity_source), 1)
  expect_gte(min(pr$proximity_target), 0)
  expect_lte(max(pr$proximity_target), 1)
})

test_that("proximity returns source proximity only", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
    balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target,
    compute = "source"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_source), 9)
  expect_equal(nrow(pr$proximity_target), NULL)
  expect_gte(min(pr$proximity_source), 0)
  expect_lte(max(pr$proximity_source), 1)
})

test_that("proximity returns target proximity only", {
  pr <- proximity(
    balassa_index = economiccomplexity_output$balassa_index,
    balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
    balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target,
    compute = "target"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_source), NULL)
  expect_equal(nrow(pr$proximity_target), 12)
  expect_gte(min(pr$proximity_target), 0)
  expect_lte(max(pr$proximity_target), 1)
})

test_that("proximity fails with NULL balassa_index", {
  expect_error(
    proximity(
      balassa_index = NULL,
      balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
      balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target
    )
  )
})

test_that("proximity fails with NULL names in balassa_sum_source/balassa_sum_target", {
  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_source = as.numeric(economiccomplexity_output$complexity_measures$balassa_sum_source$value),
      balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target
    )
  )

  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
      balassa_sum_target = as.numeric(economiccomplexity_output$complexity_measures$balassa_sum_target$value)
    )
  )
})

test_that("proximity fails with NULL balassa_sum_source/balassa_sum_target", {
  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_source = NULL,
      balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target
    )
  )

  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
      balassa_sum_target = NULL
    )
  )
})

test_that("proximity fails with NULL compute", {
  expect_error(
    proximity(
      balassa_index = economiccomplexity_output$balassa_index,
      balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
      balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target,
      compute = NULL
    )
  )
})
