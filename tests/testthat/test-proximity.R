test_that("proximity results are aligned with the expected output ", {
  pr <- ec_proximity(
    rca = ec_output_demo$rca_tbl,
    d = ec_output_demo$complexity_measures_tbl$diversity,
    u = ec_output_demo$complexity_measures_tbl$ubiquity,
    tbl = T
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_c), 4484)
  expect_equal(nrow(pr$proximity_p), 408869)
  expect_gte(min(pr$proximity_c$value), 0)
  expect_lte(max(pr$proximity_c$value), 1)
  expect_gte(min(pr$proximity_p$value), 0)
  expect_lte(max(pr$proximity_p$value), 1)
})
