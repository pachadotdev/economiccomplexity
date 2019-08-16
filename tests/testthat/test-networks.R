test_that("network results are aligned with the expected output ", {
  # igraph output ----
  net_i <- networks(
    country_proximity =
      package_output_demo$proximity_matrix$country_proximity,
    product_proximity =
      package_output_demo$proximity_matrix$product_proximity,
    countries_cutoff = 0.7,
    products_cutoff = 0.1
  )

  expect_is(net_i, "list")
  expect_equal(length(igraph::E(net_i$country_network)), 245)
  expect_equal(length(igraph::E(net_i$product_network)), 36)

  net_i_2 <- networks(
    country_proximity =
      package_output_demo$proximity_tibble$country_proximity,
    product_proximity =
      package_output_demo$proximity_tibble$product_proximity,
    countries_cutoff = 0.7,
    products_cutoff = 0.1
  )

  expect_is(net_i_2, "list")
  expect_equal(length(igraph::E(net_i_2$country_network)), 245)
  expect_equal(length(igraph::E(net_i_2$product_network)), 36)

  expect_equivalent(
    igraph::as_data_frame(net_i$country_network),
    igraph::as_data_frame(net_i_2$country_network)
  )
  expect_equivalent(
    igraph::as_data_frame(net_i$product_network),
    igraph::as_data_frame(net_i_2$product_network)
  )

  # tibble output ----
  net_t <- networks(
    country_proximity =
      package_output_demo$proximity_matrix$country_proximity,
    product_proximity =
      package_output_demo$proximity_matrix$product_proximity,
    countries_cutoff = 0.7,
    products_cutoff = 0.1,
    tbl_output = T
  )

  expect_is(net_t, "list")
  expect_equal(nrow(net_t$country_network), 245)
  expect_equal(nrow(net_t$product_network), 36)
  expect_equal(ncol(net_t$country_network), 3)
  expect_equal(ncol(net_t$product_network), 3)

  net_t_2 <- networks(
    country_proximity =
      package_output_demo$proximity_tibble$country_proximity,
    product_proximity =
      package_output_demo$proximity_tibble$product_proximity,
    countries_cutoff = 0.7,
    products_cutoff = 0.1,
    tbl_output = T
  )

  expect_is(net_t_2, "list")
  expect_equal(nrow(net_t_2$country_network), 245)
  expect_equal(nrow(net_t_2$product_network), 36)
  expect_equal(ncol(net_t_2$country_network), 3)
  expect_equal(ncol(net_t_2$product_network), 3)

  expect_equivalent(net_t$country_network, net_t_2$country_network)
  expect_equivalent(net_t$product_network, net_t_2$product_network)
})
