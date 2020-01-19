test_that("complexity_outlook works with a sparse matrix + named vector", {
  co <- complexity_outlook(
    economiccomplexity_output$balassa_index,
    economiccomplexity_output$proximity$proximity_product,
    economiccomplexity_output$complexity_measures$complexity_index_product
  )

  expect_is(co, "list")
  expect_equal(length(co$complexity_outlook_index), 226)
  expect_equal(nrow(co$complexity_outlook_gain), 226)
  expect_equal(ncol(co$complexity_outlook_gain), 785)
})

test_that("complexity_outlook fails with NULL data", {
  expect_error(
    complexity_outlook(
      NULL,
      economiccomplexity_output$proximity$proximity_product,
      economiccomplexity_output$complexity_measures$complexity_index_product
    )
  )

  expect_error(
    complexity_outlook(
      economiccomplexity_output$balassa_index,
      NULL,
      economiccomplexity_output$complexity_measures$complexity_index_product
    )
  )

  expect_error(
    complexity_outlook(
      economiccomplexity_output$balassa_index,
      economiccomplexity_output$proximity$proximity_product,
      NULL
    )
  )
})
