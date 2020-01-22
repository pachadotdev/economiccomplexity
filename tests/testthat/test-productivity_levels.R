test_that("productivity_levels works with data frame + data frame", {
  pl <- expect_warning(
    productivity_levels(
      world_trade_avg_1998_to_2000,
      world_gdp_avg_1998_to_2000
    )
  )

  expect_is(pl, "list")
  expect_equal(length(pl$productivity_level_country), 185)
  expect_equal(length(pl$productivity_level_product), 785)
})

test_that("productivity_levels works with a data frame + named vector", {
  wt <- world_trade_avg_1998_to_2000
  wt$country <- as.factor(wt$country)
  wt$product <- as.factor(wt$product)

  wt <- with(
    wt,
    Matrix::sparseMatrix(
      i = as.numeric(country),
      j = as.numeric(product),
      x = value,
      dimnames = list(levels(country), levels(product))
    )
  )

  wt <- Matrix::as.matrix(wt)

  pl <- expect_warning(
    productivity_levels(
      world_trade_avg_1998_to_2000,
      world_gdp_avg_1998_to_2000
    )
  )

  pl2 <- expect_warning(
    productivity_levels(
      wt,
      world_gdp_avg_1998_to_2000
    )
  )

  expect_is(pl2, "list")
  expect_equal(pl$productivity_level_country, pl2$productivity_level_country)
  expect_equal(pl$productivity_level_product, pl2$productivity_level_product)
  expect_equal(length(pl$productivity_level_country), 185)
  expect_equal(length(pl$productivity_level_product), 785)
})

test_that("productivity_levels fails with NULL data", {
  expect_error(
    productivity_levels(
      NULL,
      world_gdp_avg_1998_to_2000
    )
  )

  expect_error(
    productivity_levels(
      world_trade_avg_1998_to_2000,
      NULL
    )
  )
})
