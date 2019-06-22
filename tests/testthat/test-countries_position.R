test_that("countries position is aligned with the expected output", {
  # matrix output ----
  cp_m <-  countries_position(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_matrix,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    proximity_products = package_output_demo$proximity_matrix$proximity_products,
    p21 = "from",
    p22 = "to",
    v2 = "value",
    product_complexity_index = package_output_demo$complexity_measures_numeric$product_complexity_index,
    p3 = "product",
    v3 = "value"
  )

  expect_is(cp_m, "list")
  expect_is(cp_m$proximity_distance, "dgeMatrix")
  expect_is(cp_m$complexity_outlook, "numeric")
  expect_is(cp_m$complexity_outlook_gain, "dgeMatrix")
  expect_equal(nrow(cp_m$proximity_distance), 80)
  expect_equal(ncol(cp_m$proximity_distance), 11)
  expect_equal(length(cp_m$complexity_outlook), 80)
  expect_equal(nrow(cp_m$complexity_outlook_gain), 80)
  expect_equal(ncol(cp_m$complexity_outlook_gain), 11)

  cp_m_2 <-  countries_position(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_tibble,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    proximity_products = package_output_demo$proximity_tibble$proximity_products,
    p21 = "from",
    p22 = "to",
    v2 = "value",
    product_complexity_index = package_output_demo$complexity_measures_tibble$product_complexity_index,
    p3 = "product",
    v3 = "value"
  )

  expect_is(cp_m_2, "list")
  expect_is(cp_m_2$proximity_distance, "dgeMatrix")
  expect_is(cp_m_2$complexity_outlook, "numeric")
  expect_is(cp_m_2$complexity_outlook_gain, "dgeMatrix")
  expect_equal(nrow(cp_m_2$proximity_distance), 80)
  expect_equal(ncol(cp_m_2$proximity_distance), 11)
  expect_equal(length(cp_m_2$complexity_outlook), 80)
  expect_equal(nrow(cp_m_2$complexity_outlook_gain), 80)
  expect_equal(ncol(cp_m_2$complexity_outlook_gain), 11)

  # expect_equivalent(cp_m$proximity_distance, cp_m_2$proximity_distance)
  # expect_equivalent(cp_m$complexity_outlook, cp_m_2$complexity_outlook)
  # expect_equivalent(cp_m$complexity_outlook_gain, cp_m_2$complexity_outlook_gain)

  # tibble output ----
  cp_t <-  countries_position(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_matrix,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    proximity_products = package_output_demo$proximity_matrix$proximity_products,
    p21 = "from",
    p22 = "to",
    v2 = "value",
    product_complexity_index = package_output_demo$complexity_measures_numeric$product_complexity_index,
    p3 = "product",
    v3 = "value",
    tbl_output = TRUE
  )

  expect_is(cp_t, "list")
  expect_is(cp_t$proximity_distance, "data.frame")
  expect_is(cp_t$complexity_outlook, "data.frame")
  expect_is(cp_t$complexity_outlook_gain, "data.frame")
  expect_equal(nrow(cp_t$proximity_distance), 880)
  expect_equal(ncol(cp_t$proximity_distance), 3)
  expect_equal(nrow(cp_t$complexity_outlook), 80)
  expect_equal(ncol(cp_t$complexity_outlook), 2)
  expect_equal(nrow(cp_t$complexity_outlook_gain), 880)
  expect_equal(ncol(cp_t$complexity_outlook_gain), 3)

  cp_t_2 <-  countries_position(
    revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_tibble,
    c1 = "country",
    p1 = "product",
    v1 = "value",
    proximity_products = package_output_demo$proximity_tibble$proximity_products,
    p21 = "from",
    p22 = "to",
    v2 = "value",
    product_complexity_index = package_output_demo$complexity_measures_tibble$product_complexity_index,
    p3 = "product",
    v3 = "value",
    tbl_output = TRUE
  )

  expect_is(cp_t_2, "list")
  expect_is(cp_t_2$proximity_distance, "data.frame")
  expect_is(cp_t_2$complexity_outlook, "data.frame")
  expect_is(cp_t_2$complexity_outlook_gain, "data.frame")
  expect_equal(nrow(cp_t_2$proximity_distance), 880)
  expect_equal(ncol(cp_t_2$proximity_distance), 3)
  expect_equal(nrow(cp_t_2$complexity_outlook), 80)
  expect_equal(ncol(cp_t_2$complexity_outlook), 2)
  expect_equal(nrow(cp_t_2$complexity_outlook_gain), 880)
  expect_equal(ncol(cp_t_2$complexity_outlook_gain), 3)

  # expect_equivalent(cp_t$proximity_distance, cp_t_2$proximity_distance)
  # expect_equivalent(cp_t$complexity_outlook, cp_t_2$complexity_outlook)
  # expect_equivalent(cp_t$complexity_outlook_gain, cp_t_2$complexity_outlook_gain)
})
