test_that("fitness method works in complexity_measures", {
  cm <- complexity_measures(
    balassa_index = economiccomplexity_output$balassa_index,
    method = "fitness",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_country, "numeric")
  expect_is(cm$complexity_index_product, "numeric")
  expect_equal(length(cm$complexity_index_country), 9)
  expect_equal(length(cm$complexity_index_product), 12)
})

test_that("reflections method works in complexity_measures", {
  cm <- complexity_measures(
    balassa_index = economiccomplexity_output$balassa_index,
    method = "reflections",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_country, "numeric")
  expect_is(cm$complexity_index_product, "numeric")
  expect_equal(length(cm$complexity_index_country), 9)
  expect_equal(length(cm$complexity_index_product), 12)
})

test_that("eigenvalues method works in complexity_measures", {
  cm <- complexity_measures(
    balassa_index = economiccomplexity_output$balassa_index,
    method = "eigenvalues",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_country, "numeric")
  expect_is(cm$complexity_index_product, "numeric")
  expect_equal(length(cm$complexity_index_country), 9)
  expect_equal(length(cm$complexity_index_product), 12)
})

test_that("complexity_measures fails with NULL balassa_index", {
  expect_error(
    complexity_measures(
      balassa_index = NULL
    )
  )
})

test_that("complexity_measures fails with NULL iterations", {
  expect_error(
    complexity_measures(
      balassa_index = economiccomplexity_output$balassa_index,
      method = "fitness",
      iterations = NULL,
      extremality = 2
    )
  )
})

test_that("complexity_measures fails with iterations < 2", {
  expect_error(
    complexity_measures(
      balassa_index = economiccomplexity_output$balassa_index,
      method = "fitness",
      iterations = 1,
      extremality = 2
    )
  )
})

test_that("complexity_measures fails with NULL extremality", {
  expect_error(
    complexity_measures(
      balassa_index = economiccomplexity_output$balassa_index,
      method = "fitness",
      iterations = 10,
      extremality = NULL
    )
  )
})


test_that("complexity_measures fails with NULL method", {
  expect_error(
    complexity_measures(
      balassa_index = economiccomplexity_output$balassa_index,
      method = NULL
    )
  )
})
