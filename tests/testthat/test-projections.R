test_that("projection returns a simplified network with adecuate parameters", {
  # this intentionally simplifies very little and removes few links
  net <- projections(
    proximity_country = economiccomplexity_output$proximity$proximity_country,
    proximity_product = economiccomplexity_output$proximity$proximity_product,
    tolerance = 0.25,
    avg_links = 100
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_country)), 2780)
  expect_equal(length(E(net$network_product)), 1339)
})

test_that("projection returns the spanning tree with extreme parameters", {
  # dim(world_trade_avg_1998_to_2000) is 226 x 785
  # the spanning trees, by definition, shall contain 225 and 784 links each
  net <- expect_warning(
    projections(
      proximity_country = economiccomplexity_output$proximity$proximity_country,
      proximity_product = economiccomplexity_output$proximity$proximity_product,
      tolerance = 1,
      avg_links = 1
    )
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_country)), 225)
  expect_equal(length(E(net$network_product)), 784)
})

test_that("projection returns country projection only", {
  # just spanning tree for speed
  net <- expect_warning(
    projections(
      proximity_country = economiccomplexity_output$proximity$proximity_country,
      proximity_product = economiccomplexity_output$proximity$proximity_product,
      tolerance = 1,
      avg_links = 1,
      compute = "country"
    )
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_country)), 225)
  expect_equal(net$network_product, NULL)
})

test_that("projection returns product projection only", {
  # just spanning tree for speed
  net <- expect_warning(
    projections(
      proximity_country = economiccomplexity_output$proximity$proximity_country,
      proximity_product = economiccomplexity_output$proximity$proximity_product,
      tolerance = 1,
      avg_links = 1,
      compute = "product"
    )
  )

  expect_is(net, "list")
  expect_equal(net$network_country, NULL)
  expect_equal(length(E(net$network_product)), 784)
})

test_that("projection fails with proximity_country/proximity_product", {
  expect_error(
    projections(
      proximity_country = NULL,
      proximity_product = economiccomplexity_output$proximity$proximity_product
    )
  )

  expect_error(
    projections(
      proximity_country = economiccomplexity_output$proximity$proximity_country,
      proximity_product = NULL
    )
  )
})

test_that("projection fails with NULL avg_links", {
  expect_error(
    projections(
      proximity_country = economiccomplexity_output$proximity$proximity_country,
      proximity_product = economiccomplexity_output$proximity$proximity_product,
      avg_links = NULL
    )
  )
})

test_that("projection fails with NULL compute ", {
  expect_error(
    projections(
      proximity_country = economiccomplexity_output$proximity$proximity_country,
      proximity_product = economiccomplexity_output$proximity$proximity_product,
      avg_links = 4,
      compute = NULL
    )
  )
})
