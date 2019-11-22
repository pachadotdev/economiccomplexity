test_that("complexity output is aligned with the expected output ", {
  cf <- complexity(ec_output_demo$rca)

  expect_is(cf, "list")
  expect_is(cf$country_complexity, "data.frame")
  expect_is(cf$country_diversity, "data.frame")
  expect_is(cf$product_complexity, "data.frame")
  expect_is(cf$product_ubiquity, "data.frame")
  expect_equal(nrow(cf$country_complexity), 158)
  expect_equal(nrow(cf$country_diversity), 158)
  expect_equal(nrow(cf$product_complexity), 991)
  expect_equal(nrow(cf$product_ubiquity), 991)
})
