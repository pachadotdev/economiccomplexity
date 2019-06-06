test_that("proximity results are aligned with the expected output ", {
  # matrix output ----
  networks_m <- networks(
    proximity_matrices_2017$countries_proximity,
    proximity_matrices_2017$countries_proximity
  )
  expect_is(networks_m, "list")
  expect_equal(length(igraph::E(networks_m$countries_network)), 962)
  expect_equal(length(igraph::E(networks_m$products_network)), 223)

  # tibble output ----
  networks_t <- networks(
    proximity_matrices_2017$countries_proximity,
    proximity_matrices_2017$products_proximity,
    tbl_output = T
  )
  expect_is(networks_t, "list")
  expect_equal(nrow(networks_t$countries_network), 962)
  expect_equal(nrow(networks_t$products_network), 1570)
  expect_equal(ncol(networks_t$countries_network), 3)
  expect_equal(ncol(networks_t$products_network), 3)
})
