test_that("productivity_levels works with a sparse matrix + named vector", {
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

test_that("productivity_levels works with a dense matrix + named vector", {
  pl <- expect_warning(
    productivity_levels(
      as.matrix(world_trade_avg_1998_to_2000),
      world_gdp_avg_1998_to_2000
    )
  )

  expect_is(pl, "list")
  expect_equal(length(pl$productivity_level_country), 185)
  expect_equal(length(pl$productivity_level_product), 785)
})

test_that("productivity_levels works with a data frame + named vector", {
  wt <- Matrix::as.matrix(world_trade_avg_1998_to_2000)
  wt <- as.data.frame(as.table(wt), stringsAsFactors = FALSE)

  pl <- expect_warning(
    productivity_levels(
      world_trade_avg_1998_to_2000,
      world_gdp_avg_1998_to_2000
    )
  )

  pl2 <- expect_warning(
    productivity_levels(
      wt,
      world_gdp_avg_1998_to_2000,
      country = "Var1", product = "Var2", value = "Freq"
    )
  )

  expect_is(pl2, "list")
  expect_equal(pl$productivity_level_country, pl2$productivity_level_country)
  expect_equal(pl$productivity_level_product, pl2$productivity_level_product)
  expect_equal(length(pl$productivity_level_country), 185)
  expect_equal(length(pl$productivity_level_product), 785)
})

test_that("productivity_levels works with a data frame + data frame", {
  set.seed(1810)

  wt <- Matrix::as.matrix(world_trade_avg_1998_to_2000)
  wt <- as.data.frame(as.table(wt), stringsAsFactors = FALSE)

  wg <- world_gdp_avg_1998_to_2000
  wg <- as.data.frame(as.table(wg), stringsAsFactors = FALSE)

  pl <- expect_warning(
    productivity_levels(
      world_trade_avg_1998_to_2000,
      world_gdp_avg_1998_to_2000
    )
  )

  pl2 <- expect_warning(
    productivity_levels(
      wt,
      wg,
      country = "Var1", product = "Var2", value = "Freq"
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
