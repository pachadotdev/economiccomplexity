test_that("projection returns a simplified network with adecuate parameters", {
  net <- projections(
    proximity_source = economiccomplexity_output$proximity$proximity_source,
    proximity_target = economiccomplexity_output$proximity$proximity_target,
    tolerance = 0.1,
    avg_links = 10
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_source)), 21)
  expect_equal(length(E(net$network_target)), 40)
})

test_that("projection returns the spanning tree with extreme parameters", {
  net <- expect_warning(
    projections(
      proximity_source = economiccomplexity_output$proximity$proximity_source,
      proximity_target = economiccomplexity_output$proximity$proximity_target,
      tolerance = 1,
      avg_links = 1
    )
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_source)), 8)
  expect_equal(length(E(net$network_target)), 11)
})

test_that("projection returns source projection only", {
  # just spanning tree for speed
  net <- expect_warning(
    projections(
      proximity_source = economiccomplexity_output$proximity$proximity_source,
      proximity_target = economiccomplexity_output$proximity$proximity_target,
      tolerance = 1,
      avg_links = 1,
      compute = "source"
    )
  )

  expect_is(net, "list")
  expect_equal(length(E(net$network_source)), 8)
  expect_equal(net$network_target, NULL)
})

test_that("projection returns target projection only", {
  # just spanning tree for speed
  net <- expect_warning(
    projections(
      proximity_source = economiccomplexity_output$proximity$proximity_source,
      proximity_target = economiccomplexity_output$proximity$proximity_target,
      tolerance = 1,
      avg_links = 1,
      compute = "target"
    )
  )

  expect_is(net, "list")
  expect_equal(net$network_source, NULL)
  expect_equal(length(E(net$network_target)), 11)
})

test_that("projection fails with proximity_source/proximity_target", {
  expect_error(
    projections(
      proximity_source = NULL,
      proximity_target = economiccomplexity_output$proximity$proximity_target
    )
  )

  expect_error(
    projections(
      proximity_source = economiccomplexity_output$proximity$proximity_source,
      proximity_target = NULL
    )
  )
})

test_that("projection fails with NULL avg_links", {
  expect_error(
    projections(
      proximity_source = economiccomplexity_output$proximity$proximity_source,
      proximity_target = economiccomplexity_output$proximity$proximity_target,
      avg_links = NULL
    )
  )
})

test_that("projection fails with NULL compute ", {
  expect_error(
    projections(
      proximity_source = economiccomplexity_output$proximity$proximity_source,
      proximity_target = economiccomplexity_output$proximity$proximity_target,
      avg_links = 4,
      compute = NULL
    )
  )
})
