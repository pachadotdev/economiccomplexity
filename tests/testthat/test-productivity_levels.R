test_that("productivity_levels works with a sparse matrix + named vector", {
  set.seed(1810)

  pl <- productivity_levels(
    galactic_federation,
    setNames(rnorm(1:nrow(galactic_federation), 1000, 200), rownames(galactic_federation))
  )

  expect_is(pl, "list")
  expect_equal(length(pl$productivity_level_country), 9)
  expect_equal(length(pl$productivity_level_product), 12)
})

test_that("productivity_levels works with a dense matrix + named vector", {
  set.seed(1810)

  pl <- productivity_levels(
    as.matrix(galactic_federation),
    setNames(rnorm(1:nrow(galactic_federation), 1000, 200), rownames(galactic_federation))
  )

  expect_is(pl, "list")
  expect_equal(length(pl$productivity_level_country), 9)
  expect_equal(length(pl$productivity_level_product), 12)
})

test_that("productivity_levels works with a data frame + named vector", {
  set.seed(1810)

  gf <- Matrix::as.matrix(galactic_federation)
  gf <- as.data.frame(as.table(gf), stringsAsFactors = FALSE)

  gf2 <- setNames(rnorm(1:nrow(galactic_federation), 1000, 200), rownames(galactic_federation))

  pl <- productivity_levels(
    galactic_federation,
    gf2
  )

  pl2 <- productivity_levels(
    gf,
    gf2,
    country = "Var1", product = "Var2", value = "Freq"
  )

  expect_is(pl2, "list")
  expect_equal(pl$productivity_level_country, pl2$productivity_level_country)
  expect_equal(pl$productivity_level_product, pl2$productivity_level_product)
  expect_equal(length(pl$productivity_level_country), 9)
  expect_equal(length(pl$productivity_level_product), 12)
})

test_that("productivity_levels works with a data frame + data frame", {
  set.seed(1810)

  gf <- Matrix::as.matrix(galactic_federation)
  gf <- as.data.frame(as.table(gf), stringsAsFactors = FALSE)

  gf2 <- setNames(rnorm(1:nrow(galactic_federation), 1000, 200), rownames(galactic_federation))
  gf2 <- as.data.frame(as.table(gf2), stringsAsFactors = FALSE)

  pl <- productivity_levels(
    galactic_federation,
    gf2,
    country = "Var1", value = "Freq"
  )

  pl2 <- productivity_levels(
    gf,
    gf2,
    country = "Var1", product = "Var2", value = "Freq"
  )

  expect_is(pl2, "list")
  expect_equal(pl$productivity_level_country, pl2$productivity_level_country)
  expect_equal(pl$productivity_level_product, pl2$productivity_level_product)
  expect_equal(length(pl$productivity_level_country), 9)
  expect_equal(length(pl$productivity_level_product), 12)
})

test_that("productivity_levels return warning with missing country", {
  set.seed(1810)

  incomplete_gdp <- setNames(rnorm(1:nrow(galactic_federation), 1000, 200), rownames(galactic_federation))
  incomplete_gdp <- incomplete_gdp[1:(length(incomplete_gdp) - 1)]

  expect_warning(
    productivity_levels(
      galactic_federation,
      incomplete_gdp
    )
  )
})

test_that("productivity_levels fails with NULL data", {
  set.seed(1810)

  expect_error(
    productivity_levels(
      NULL,
      setNames(rnorm(1:nrow(galactic_federation), 1000, 200), rownames(galactic_federation))
    )
  )
  expect_error(
    productivity_levels(
      galactic_federation,
      NULL
    )
  )
})

