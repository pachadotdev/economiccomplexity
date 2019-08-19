test_that("network results are aligned with the expected output ", {
  net <- ec_networks(
    pc = ec_output_demo$proximity_tbl$proximity_c,
    pp = ec_output_demo$proximity_tbl$proximity_p,
    cutoff_c = 1,
    cutoff_p = 1,
    tbl = T
  )

  expect_is(net, "list")
  expect_equal(nrow(net$network_country), 98)
  expect_equal(nrow(net$network_product), 995)
  expect_equal(ncol(net$network_country), 3)
  expect_equal(ncol(net$network_product), 3)
})
