#' Countries position
#'
#' @export
#' @param revealed_comparative_advantage matrix or tibble/data.frame (e.g. the output of
#' \code{revealed_comparative_advantage()}).
#' If the input is a matrix it must be a zero/one matrix with countries in rows and products in columns.
#' If the input is a tibble/data.frame it must contain at least three columns with countries, products and
#' values.
#' @param c1 string to indicate the column that contains exporting countries in revealed_comparative_advantage
#' (set to "country" by default)
#' @param p1 string to indicate the column that contains exported products in revealed_comparative_advantage
#' (set to "product" by default)
#' @param v1 string to indicate the column that contains traded values in revealed_comparative_advantage
#' (set to "value" by default)
#' @param proximity_products matrix or tibble/data.frame (e.g. the output of
#' \code{proximity_matrices()}).
#' If the input is a matrix it must be a numeric matrix with products in both
#' rows and columns.
#' If the input is a tibble/data.frame it must contain at least three columns columns with
#' products (twice) and values.
#' @param p21 string to indicate the first column that contains exported products in proximity_products
#' (set to "from" by default)
#' @param p22 string to indicate the second column that contains exported products in proximity_products
#' (set to "to" by default)
#' @param v2 string to indicate the column that contains proximity values in proximity_products
#' (set to "value" by default)
#' @param product_complexity_index numeric vector or tibble/data.frame \code{complexity_measures()}).
#' If the input is a vector it must be numeric with optional names.
#' If the input is a tibble/data.frame it must contain at least two columns columns with products and values.
#' @param p3 string to indicate the column that contains exported products in proximity_products (e.g. "product")
#' @param v3 string to indicate the column that contains proximity values in proximity_products (e.g. "value")
#' from \code{complexity_measures()})
#' @param tbl_output logical value to use tibble output instead of a matrix output (set to FALSE by default)
#' @importFrom magrittr %>%
#' @importFrom dplyr select pull
#' @importFrom tidyr spread
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#' @examples
#' countries_position(
#'     revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_matrix,
#'     c1 = "country",
#'     p1 = "product",
#'     v1 = "value",
#'     proximity_products = package_output_demo$proximity_matrix$products_proximity,
#'     p21 = "from",
#'     p22 = "to",
#'     v2 = "value",
#'     product_complexity_index = package_output_demo$complexity_measures_numeric$product_complexity_index,
#'     p3 = "product",
#'     v3 = "value"
#' )
#' @references
#' For more information on proximity distance, complexity outlook, complexity outlook gain and its
#' applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#' @keywords functions

countries_position <- function(revealed_comparative_advantage = NULL,
                               c1 = "country",
                               p1 = "product",
                               v1 = "value",
                               proximity_products = NULL,
                               p21 = "from",
                               p22 = "to",
                               v2 = "value",
                               product_complexity_index = NULL,
                               p3 = "product",
                               v3 = "value",
                               tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(revealed_comparative_advantage) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix", "dgCMatrix") == FALSE)) {
    stop("revealed_comparative_advantage must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(proximity_products) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix", "dgCMatrix") == FALSE)) {
    stop("proximity_products must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(product_complexity_index) %in% c("data.frame", "numeric") == FALSE)) {
    stop("product_complexity_index must be a tibble/data.frame or numeric")
  }

  if (is.data.frame(revealed_comparative_advantage)) {
    revealed_comparative_advantage <- tidyr::spread(revealed_comparative_advantage, !!sym(p1), !!sym(v1))

    revealed_comparative_advantage_names <- dplyr::select(revealed_comparative_advantage, !!sym(c1)) %>%
      dplyr::pull()

    revealed_comparative_advantage <- dplyr::select(revealed_comparative_advantage, -!!sym(c1)) %>%
      as.matrix()

    rownames(revealed_comparative_advantage) <- revealed_comparative_advantage_names
  }

  if (is.matrix(revealed_comparative_advantage)) {
    revealed_comparative_advantage[is.na(revealed_comparative_advantage)] <- 0
    revealed_comparative_advantage <- Matrix::Matrix(revealed_comparative_advantage, sparse = T)
  }

  if (is.data.frame(proximity_products)) {
    proximity_products_rows <- dplyr::select(proximity_products, !!sym(p21)) %>% dplyr::pull()
    proximity_products_cols <- dplyr::select(proximity_products, !!sym(p22)) %>% dplyr::pull()

    proximity_products_cols_rows <- sort(
      unique(c(proximity_products_rows, proximity_products_cols))
    )

    proximity_products_expand <- expand.grid(
      from = proximity_products_cols_rows,
      to = proximity_products_cols_rows,
      stringsAsFactors = F
    )

    colnames(proximity_products_expand) <- c(p21,p22)

    proximity_products <- proximity_products_expand %>%
      dplyr::left_join(proximity_products)

    proximity_products <- tidyr::spread(proximity_products, !!sym(p22), !!sym(v2))
    proximity_products_rows <- dplyr::select(proximity_products, !!sym(p21)) %>% dplyr::pull()

    proximity_products <- proximity_products %>%
      dplyr::select(-!!sym(p21)) %>%
      as.matrix()

    proximity_products[is.na(proximity_products)] <- 0
    rownames(proximity_products) <- proximity_products_rows

    diag(proximity_products) <- 1
    proximity_products[upper.tri(proximity_products, diag = F)] <- t(proximity_products)[upper.tri(proximity_products, diag = F)]
  }

  if (is.matrix(proximity_products)) {
    proximity_products <- Matrix::Matrix(proximity_products, sparse = T)
  }

  if (is.data.frame(product_complexity_index)) {
    product_complexity_index_values <- dplyr::select(product_complexity_index, !!sym(v3)) %>% dplyr::pull()
    product_complexity_index_names <- dplyr::select(product_complexity_index, !!sym(p3)) %>% dplyr::pull()

    product_complexity_index <- product_complexity_index_values
    names(product_complexity_index) <- product_complexity_index_names

    product_complexity_index <- product_complexity_index[sort(names(product_complexity_index))]
  }

  dcp <- ((1 - revealed_comparative_advantage) %*% proximity_products) / (Matrix::Matrix(1, nrow = nrow(revealed_comparative_advantage), ncol = ncol(revealed_comparative_advantage)) %*% proximity_products)

  coc <- Matrix::colSums(Matrix::t((1 - dcp) * (1 - revealed_comparative_advantage)) * product_complexity_index)

  cogc <- Matrix::t((Matrix::t((1 - dcp) %*% (proximity_products / Matrix::colSums(proximity_products))) * product_complexity_index) - (Matrix::t(1 - dcp) * product_complexity_index))

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
