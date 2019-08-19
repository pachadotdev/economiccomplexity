test_that("countries position is aligned with the expected output", {
  cp <- ec_countries_position(
    rca = ec_output_demo$rca_tbl,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    pp = ec_output_demo$proximity_tbl$proximity_p,
    p21 = "from",
    p22 = "to",
    v2 = "value",
    pci = ec_output_demo$complexity_measures_tbl$complexity_index_p,
    p3 = "product",
    v3 = "value",
    tbl = T
  )

  expect_is(cp, "list")
  expect_is(cp$proximity_distance, "data.frame")
  expect_is(cp$complexity_outlook, "data.frame")
  expect_is(cp$complexity_outlook_gain, "data.frame")
  expect_equal(nrow(cp$proximity_distance), 156578)
  expect_equal(nrow(cp$complexity_outlook), 158)
  expect_equal(nrow(cp$complexity_outlook_gain), 156578)
})
