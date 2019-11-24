test_that("network results are aligned with the expected output ", {
  net <- projections(
    proximity_c = ec_output_demo$proximity$proximity_c,
    proximity_p = ec_output_demo$proximity$proximity_p,
    cutoff_c = 1,
    cutoff_p = 1
  )

  expect_is(net, "list")
  expect_equal(nrow(net$projection_c), 98)
  expect_equal(nrow(net$projection_p), 995)
  expect_equal(ncol(net$projection_c), 3)
  expect_equal(ncol(net$projection_p), 3)
})
