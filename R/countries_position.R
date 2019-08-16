#' Countries position
#'
#' @export
#' @param rca matrix or tibble/data.frame (e.g. the
#' output of \code{revealed_comparative_advantage()}).
#' If the input is a matrix it must be a zero/one matrix with countries in rows
#' and products in columns.
#' If the input is a tibble/data.frame it must contain at least three columns
#' with countries, products and values.
#' @param country1 string to indicate the column that contains exporting
#' countries in rca (set to "country" by default)
#' @param product1 string to indicate the column that contains exported products
#' in rca (set to "product" by default)
#' @param value1 string to indicate the column that contains traded values in
#' rca (set to "value" by default)
#' @param proximity_products matrix or tibble/data.frame (e.g. the output of
#' \code{proximity_matrices()}).
#' If the input is a matrix it must be a numeric matrix with products in both
#' rows and columns.
#' If the input is a tibble/data.frame it must contain at least three columns
#' columns with products (twice) and values.
#' @param product21 string to indicate the first column that contains exported
#' products in proximity_products (set to "from" by default)
#' @param product22 string to indicate the second column that contains exported
#' products in proximity_products (set to "to" by default)
#' @param value2 string to indicate the column that contains proximity values in
#' proximity_products (set to "value" by default)
#' @param product_complexity_index numeric vector or tibble/data.frame
#' \code{complexity_measures()}).
#' If the input is a vector it must be numeric with optional names.
#' If the input is a tibble/data.frame it must contain at least two columns
#' with products and values.
#' @param product3 string to indicate the column that contains exported products
#' in proximity_products (e.g. "product")
#' @param value3 string to indicate the column that contains proximity values in
#' proximity_products (e.g. "value") from \code{complexity_measures()})
#' @param tbl_output logical value to use tibble output instead of a matrix
#' output (set to FALSE by default)
#' @importFrom magrittr %>%
#' @importFrom dplyr select pull
#' @importFrom tidyr spread
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#' @examples
#' countries_position(
#'   rca = package_output_demo$revealed_comparative_advantage_matrix,
#'   country1 = "country",
#'   product1 = "product",
#'   value1 = "value",
#'   proximity_products =
#'    package_output_demo$proximity_matrix$proximity_products,
#'   product21 = "from",
#'   product22 = "to",
#'   value2 = "value",
#'   product_complexity_index =
#'    package_output_demo$complexity_measures_numeric$product_complexity_index,
#'   product3 = "product",
#'   value3 = "value",
#'   tbl_output = TRUE
#' )
#' @references
#' For more information on proximity distance, complexity outlook, complexity
#' outlook gain and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

countries_position <- function(rca = NULL,
                               country1 = "country",
                               product1 = "product",
                               value1 = "value",
                               proximity_products = NULL,
                               product21 = "from",
                               product22 = "to",
                               value2 = "value",
                               product_complexity_index = NULL,
                               product3 = "product",
                               value3 = "value",
                               tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(rca) %in%
          c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
            "dgCMatrix") == FALSE)) {
    stop("rca must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(proximity_products) %in%
          c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
            "dgCMatrix") == FALSE)) {
    stop("proximity_products must be a tibble/data.frame or a dense/sparse
         matrix")
  }

  if (all(class(product_complexity_index) %in% c("data.frame",
                                                 "numeric") == FALSE)) {
    stop("product_complexity_index must be a tibble/data.frame or numeric")
  }

  if (is.data.frame(rca)) {
    rca <- tidyr::spread(rca, !!sym(product1), !!sym(value1))

    rca_names <- dplyr::select(rca, !!sym(country1)) %>%
      dplyr::pull()

    rca <- dplyr::select(rca, -!!sym(country1)) %>%
      as.matrix()

    rownames(rca) <- rca_names
  }

  if (is.matrix(rca)) {
    rca[is.na(rca)] <- 0
    rca <- Matrix::Matrix(rca, sparse = TRUE)
  }

  if (is.data.frame(proximity_products)) {
    proximity_products_rows <- dplyr::select(proximity_products,
                                             !!sym(product21)) %>% dplyr::pull()

    proximity_products_cols <- dplyr::select(proximity_products,
                                             !!sym(product22)) %>% dplyr::pull()

    proximity_products_cols_rows <- sort(
      unique(c(proximity_products_rows, proximity_products_cols))
    )

    proximity_products_expand <- expand.grid(
      from = proximity_products_cols_rows,
      to = proximity_products_cols_rows,
      stringsAsFactors = FALSE
    )

    colnames(proximity_products_expand) <- c(product21, product22)

    proximity_products <- proximity_products_expand %>%
      dplyr::left_join(proximity_products)

    proximity_products <- tidyr::spread(proximity_products,
                                        !!sym(product22), !!sym(value2))

    proximity_products_rows <- dplyr::select(proximity_products,
                                             !!sym(product21)) %>% dplyr::pull()

    proximity_products <- proximity_products %>%
      dplyr::select(-!!sym(product21)) %>%
      as.matrix()

    proximity_products[is.na(proximity_products)] <- 0
    rownames(proximity_products) <- proximity_products_rows

    diag(proximity_products) <- 1
    proximity_products[upper.tri(proximity_products, diag = FALSE)] <-
      t(proximity_products)[upper.tri(proximity_products, diag = FALSE)]
  }

  if (is.matrix(proximity_products)) {
    proximity_products <- Matrix::Matrix(proximity_products, sparse = TRUE)
  }

  if (is.data.frame(product_complexity_index)) {
    product_complexity_index_values <- dplyr::select(product_complexity_index,
                                                     !!sym(value3)) %>%
      dplyr::pull()

    product_complexity_index_names <- dplyr::select(product_complexity_index,
                                                    !!sym(product3)) %>%
      dplyr::pull()

    product_complexity_index <- product_complexity_index_values
    names(product_complexity_index) <- product_complexity_index_names

    product_complexity_index <- product_complexity_index[
      sort(names(product_complexity_index))]
  }

  dcp <- ((1 - rca) %*% proximity_products) /
    (Matrix::Matrix(1, nrow = nrow(rca), ncol = ncol(rca)) %*%
       proximity_products)

  coc <- Matrix::colSums(Matrix::t((1 - dcp) * (1 - rca)) *
            product_complexity_index)

  cogc <- Matrix::t((Matrix::t((1 - dcp) %*% (proximity_products /
            Matrix::colSums(proximity_products))) * product_complexity_index) -
            (Matrix::t(1 - dcp) * product_complexity_index))

  if (tbl_output == TRUE) {
    dcp <- as.matrix(dcp) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(country = rownames(dcp)) %>%
      tidyr::gather(!!sym("product"), !!sym("value"), -!!sym("country"))

    coc <- tibble::tibble(value = coc) %>%
      dplyr::mutate(product = names(coc)) %>%
      dplyr::select(!!sym("product"), !!sym("value")) %>%
      dplyr::arrange(-!!sym("value"))

    cogc <- as.matrix(cogc) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(country = rownames(cogc)) %>%
      tidyr::gather(!!sym("product"), !!sym("value"), -!!sym("country"))
  }

  return(
    list(
      proximity_distance = dcp,
      complexity_outlook = coc,
      complexity_outlook_gain = cogc
    )
  )
}
