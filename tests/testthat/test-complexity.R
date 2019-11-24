test_that("complexity output is aligned with the expected output ", {
  cf <- complexity(ec_output_demo$rca)

  expect_is(cf, "list")
  expect_is(cf$complexity_c, "data.frame")
  expect_is(cf$complexity_p, "data.frame")
  expect_is(cf$diversity, "data.frame")
  expect_is(cf$ubiquity, "data.frame")
  expect_equal(nrow(cf$complexity_c), 158)
  expect_equal(nrow(cf$diversity), 158)
  expect_equal(nrow(cf$complexity_p), 991)
  expect_equal(nrow(cf$ubiquity), 991)
})
