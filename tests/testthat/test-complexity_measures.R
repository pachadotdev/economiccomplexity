test_that("complexity measures are aligned with the expected output", {
  # numeric output ----
  cm_n <- complexity_measures(
    package_output_demo$revealed_comparative_advantage_matrix
  )

  expect_is(cm_n, "list")
  expect_is(cm_n$economic_complexity_index, "numeric")
  expect_is(cm_n$product_complexity_index, "numeric")
  expect_is(cm_n$diversity, "numeric")
  expect_is(cm_n$ubiquity, "numeric")
  expect_equal(length(cm_n$economic_complexity_index), 80)
  expect_equal(length(cm_n$product_complexity_index), 11)
  expect_equal(length(cm_n$diversity), 80)
  expect_equal(length(cm_n$ubiquity), 11)

  cm_n_2 <- complexity_measures(
    package_output_demo$revealed_comparative_advantage_tibble
  )

  expect_is(cm_n, "list")
  expect_is(cm_n$economic_complexity_index, "numeric")
  expect_is(cm_n$product_complexity_index, "numeric")
  expect_is(cm_n$diversity, "numeric")
  expect_is(cm_n$ubiquity, "numeric")
  expect_equal(length(cm_n$economic_complexity_index), 80)
  expect_equal(length(cm_n$product_complexity_index), 11)
  expect_equal(length(cm_n$diversity), 80)
  expect_equal(length(cm_n$ubiquity), 11)

  expect_equal(cm_n$economic_complexity_index, cm_n$economic_complexity_index)
  expect_equal(cm_n$product_complexity_index, cm_n$product_complexity_index)
  expect_equal(cm_n$diversity, cm_n$diversity)
  expect_equal(cm_n$ubiquity, cm_n$ubiquity)

  # tibble output ----
  cm_t <- complexity_measures(
    package_output_demo$revealed_comparative_advantage_matrix,
    tbl_output = T
  )

  expect_is(cm_t, "list")
  expect_is(cm_t$economic_complexity_index, "data.frame")
  expect_is(cm_t$product_complexity_index, "data.frame")
  expect_is(cm_t$diversity, "data.frame")
  expect_is(cm_t$ubiquity, "data.frame")
  expect_equal(nrow(cm_t$economic_complexity_index), 80)
  expect_equal(ncol(cm_t$economic_complexity_index), 2)
  expect_equal(nrow(cm_t$product_complexity_index), 11)
  expect_equal(ncol(cm_t$product_complexity_index), 2)
  expect_equal(nrow(cm_t$diversity), 80)
  expect_equal(ncol(cm_t$diversity), 2)
  expect_equal(nrow(cm_t$ubiquity), 11)
  expect_equal(ncol(cm_t$ubiquity), 2)

  cm_t_2 <- complexity_measures(
    package_output_demo$revealed_comparative_advantage_tibble,
    tbl_output = T
  )

  expect_is(cm_t, "list")
  expect_is(cm_t$economic_complexity_index, "data.frame")
  expect_is(cm_t$product_complexity_index, "data.frame")
  expect_is(cm_t$diversity, "data.frame")
  expect_is(cm_t$ubiquity, "data.frame")
  expect_equal(nrow(cm_t$economic_complexity_index), 80)
  expect_equal(ncol(cm_t$economic_complexity_index), 2)
  expect_equal(nrow(cm_t$product_complexity_index), 11)
  expect_equal(ncol(cm_t$product_complexity_index), 2)
  expect_equal(nrow(cm_t$diversity), 80)
  expect_equal(ncol(cm_t$diversity), 2)
  expect_equal(nrow(cm_t$ubiquity), 11)
  expect_equal(ncol(cm_t$ubiquity), 2)

  expect_equal(
    cm_t$economic_complexity_index %>% mutate(value = round(value, 4)),
    cm_t_2$economic_complexity_index %>% mutate(value = round(value, 4))
  )
  expect_equal(
    cm_t$product_complexity_index %>% mutate(value = round(value, 4)),
    cm_t_2$product_complexity_index %>% mutate(value = round(value, 4))
  )
  expect_equal(cm_t$diversity, cm_t_2$diversity)
  expect_equal(cm_t$ubiquity, cm_t_2$ubiquity)
})
