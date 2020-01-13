test_that("projection returns a simplified network with adecuate parameters", {
  net <- projections(
    proximity_country = economiccomplexity_output$proximity$proximity_country,
    proximity_product = economiccomplexity_output$proximity$proximity_product,
    tolerance = 0.1,
    avg_links = 10
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_country)), 21)
  expect_equal(length(E(net$network_product)), 40)
})

test_that("projection returns the spanning tree with extreme parameters", {
  net <- expect_warning(
    projections(
      proximity_country = economiccomplexity_output$proximity$proximity_country,
      proximity_product = economiccomplexity_output$proximity$proximity_product,
      tolerance = 1,
      avg_links = 1
    )
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_country)), 8)
  expect_equal(length(E(net$network_product)), 11)
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
  expect_equal(length(E(net$network_country)), 8)
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
  expect_equal(length(E(net$network_product)), 11)
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
