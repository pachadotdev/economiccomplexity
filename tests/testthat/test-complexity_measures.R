test_that("complexity measures are aligned with the expected output", {
  cm <- ec_complexity_measures(
    ec_output_demo$rca_tbl,
    tbl = T
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_c, "data.frame")
  expect_is(cm$complexity_index_p, "data.frame")
  expect_is(cm$diversity, "data.frame")
  expect_is(cm$ubiquity, "data.frame")
  expect_equal(nrow(cm$complexity_index_c), 158)
  expect_equal(nrow(cm$complexity_index_p), 991)
  expect_equal(nrow(cm$diversity), 158)
  expect_equal(nrow(cm$ubiquity), 991)
})
