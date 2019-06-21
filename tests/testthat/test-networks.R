test_that("network results are aligned with the expected output ", {
  # matrix output ----
  net_m <- networks(
    countries_proximity = package_output_demo$proximity_matrix$countries_proximity,
    products_proximity = package_output_demo$proximity_matrix$products_proximity,
    c_cutoff = 0.7,
    p_cutoff = 0.1
  )
  expect_is(net_m, "list")
  expect_equal(length(igraph::E(net_m$countries_network)), 245)
  expect_equal(length(igraph::E(net_m$products_network)), 36)

  # tibble output ----
  net_t <- networks(
    countries_proximity = package_output_demo$proximity_matrix$countries_proximity,
    products_proximity = package_output_demo$proximity_matrix$products_proximity,
    c_cutoff = 0.7,
    p_cutoff = 0.1,
    tbl_output = T
  )
  expect_is(net_t, "list")
  expect_equal(nrow(net_t$countries_network), 245)
  expect_equal(nrow(net_t$products_network), 36)
  expect_equal(ncol(net_t$countries_network), 3)
  expect_equal(ncol(net_t$products_network), 3)
})
