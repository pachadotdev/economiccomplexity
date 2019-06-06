test_that("proximity results are aligned with the expected output ", {
  # matrix output ----
  proximity_m <- proximity_matrices(
    d = world_rca_2017,
    diversity = complexity_measures_2017$diversity,
    ubiquity = complexity_measures_2017$ubiquity
  )
  expect_is(proximity_m, "list")
  expect_equal(nrow(proximity_m$countries_proximity), 224)
  expect_equal(ncol(proximity_m$countries_proximity), 224)
  expect_equal(nrow(proximity_m$products_proximity), 1222)
  expect_equal(ncol(proximity_m$products_proximity), 1222)
  expect_gte(min(proximity_m$countries_proximity), 0)
  expect_lte(max(proximity_m$countries_proximity), 1)
  expect_gte(min(proximity_m$products_proximity), 0)
  expect_lte(max(proximity_m$products_proximity), 1)

  # tibble output ----
  proximity_t <- proximity_matrices(
    d = world_rca_2017,
    diversity = complexity_measures_2017$diversity,
    ubiquity = complexity_measures_2017$ubiquity,
    tbl_output = T
  )
  expect_is(proximity_t, "list")
  expect_equal(nrow(proximity_t$countries_proximity), 23481)
  expect_equal(ncol(proximity_t$countries_proximity), 3)
  expect_equal(nrow(proximity_t$products_proximity), 703017)
  expect_equal(ncol(proximity_t$products_proximity), 3)
  expect_gte(min(proximity_t$countries_proximity$value), 0)
  expect_lte(max(proximity_t$countries_proximity$value), 1)
  expect_gte(min(proximity_t$products_proximity$value), 0)
  expect_lte(max(proximity_t$products_proximity$value), 1)
})
