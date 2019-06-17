test_that("countries position is aligned with the expected output", {
  # matrix output ----
  cp_m <-  countries_position(
    revealed_comparative_advantage = revealed_comparative_advantage_output,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    proximity_products = proximity_matrices_output$products_proximity,
    p21 = "from",
    p22 = "to",
    v2 = "value",
    product_complexity_index = complexity_measures_output$product_complexity_index,
    p3 = "product",
    v3 = "value"
  )
  expect_is(cp_m, "list")
  expect_is(cp_m$proximity_distance, "Matrix")
  expect_is(cp_m$complexity_outlook, "numeric")
  expect_is(cp_m$complexity_outlook_gain, "Matrix")
  expect_equal(nrow(cp_m$proximity_distance), 224)
  expect_equal(ncol(cp_m$proximity_distance), 1222)
  expect_equal(length(cp_m$complexity_outlook), 224)
  expect_equal(nrow(cp_m$complexity_outlook_gain), 224)
  expect_equal(ncol(cp_m$complexity_outlook_gain), 1222)

  # tibble output ----
  cp_t <-  countries_position(
    revealed_comparative_advantage = revealed_comparative_advantage_output,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    proximity_products = proximity_matrices_output$products_proximity,
    p21 = "from",
    p22 = "to",
    v2 = "value",
    product_complexity_index = complexity_measures_output$product_complexity_index,
    p3 = "product",
    v3 = "value",
    tbl_output = TRUE
  )
  expect_is(cp_t, "list")
  expect_is(cp_t$proximity_distance, "data.frame")
  expect_is(cp_t$complexity_outlook, "data.frame")
  expect_is(cp_t$complexity_outlook_gain, "data.frame")
  expect_equal(nrow(cp_t$proximity_distance), 273728)
  expect_equal(ncol(cp_t$proximity_distance), 3)
  expect_equal(nrow(cp_t$complexity_outlook), 224)
  expect_equal(ncol(cp_t$complexity_outlook), 2)
  expect_equal(nrow(cp_t$complexity_outlook_gain), 273728)
  expect_equal(ncol(cp_t$complexity_outlook_gain), 3)
})
