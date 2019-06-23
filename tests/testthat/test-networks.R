test_that("network results are aligned with the expected output ", {
  # igraph output ----
  net_i <- networks(
    proximity_countries = package_output_demo$proximity_matrix$proximity_countries,
    proximity_products = package_output_demo$proximity_matrix$proximity_products,
    countries_cutoff = 0.7,
    products_cutoff = 0.1
  )

  expect_is(net_i, "list")
  expect_equal(length(igraph::E(net_i$network_countries)), 245)
  expect_equal(length(igraph::E(net_i$network_products)), 36)

  net_i_2 <- networks(
    proximity_countries = package_output_demo$proximity_tibble$proximity_countries,
    proximity_products = package_output_demo$proximity_tibble$proximity_products,
    countries_cutoff = 0.7,
    products_cutoff = 0.1
  )

  expect_is(net_i_2, "list")
  expect_equal(length(igraph::E(net_i_2$network_countries)), 245)
  expect_equal(length(igraph::E(net_i_2$network_products)), 36)

  expect_equivalent(
    igraph::as_data_frame(net_i$network_countries),
    igraph::as_data_frame(net_i_2$network_countries)
  )
  expect_equivalent(
    igraph::as_data_frame(net_i$network_products),
    igraph::as_data_frame(net_i_2$network_products)
  )

  # tibble output ----
  net_t <- networks(
    proximity_countries = package_output_demo$proximity_matrix$proximity_countries,
    proximity_products = package_output_demo$proximity_matrix$proximity_products,
    countries_cutoff = 0.7,
    products_cutoff = 0.1,
    tbl_output = T
  )

  expect_is(net_t, "list")
  expect_equal(nrow(net_t$network_countries), 245)
  expect_equal(nrow(net_t$network_products), 36)
  expect_equal(ncol(net_t$network_countries), 3)
  expect_equal(ncol(net_t$network_products), 3)

  net_t_2 <- networks(
    proximity_countries = package_output_demo$proximity_tibble$proximity_countries,
    proximity_products = package_output_demo$proximity_tibble$proximity_products,
    countries_cutoff = 0.7,
    products_cutoff = 0.1,
    tbl_output = T
  )

  expect_is(net_t_2, "list")
  expect_equal(nrow(net_t_2$network_countries), 245)
  expect_equal(nrow(net_t_2$network_products), 36)
  expect_equal(ncol(net_t_2$network_countries), 3)
  expect_equal(ncol(net_t_2$network_products), 3)

  expect_equivalent(net_t$network_countries, net_t_2$network_countries)
  expect_equivalent(net_t$network_products, net_t_2$network_products)
})
