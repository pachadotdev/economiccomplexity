#' Countries position
#'
#' @export
#' @param rca matrix or tibble/data.frame in long format (e.g. the output of
#' revealed_comparative_advantage()). If it is a matrix it must be a
#' zero/one matrix with countries in the row names and products in the column names.
#' If rca is a tibble/data.frame it must contain the columns.
#' @param c1 string to indicate the column that contains exporting countries in rca (e.g. "country")
#' @param p1 string to indicate the column that contains exported products in rca (e.g. "product")
#' @param v1 string to indicate the column that contains traded values in rca(e.g. "value")
#' @param proximity_products matrix or tibble/data.frame, if d is a tibble/data.frame it must contain the columns
#' from (character/factor), to (character/factor) and value (numeric), if it is a matrix it must be a
#' numeric matrix with products in row names and column names
#' @param p21 string to indicate the column that contains exported products in proximity_products (e.g. "from")
#' @param p22 string to indicate the column that contains exported products in proximity_products (e.g. "to")
#' @param v2 string to indicate the column that contains proximity values in proximity_products (e.g. "value")
#' @param pci numeric vector with products in names (e.g. \code{product_complexity_index}
#' @param p3 string to indicate the column that contains exported products in proximity_products (e.g. "product")
#' @param v3 string to indicate the column that contains proximity values in proximity_products (e.g. "value")
#' from \code{complexity_measures()})
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom dplyr select pull
#' @importFrom tidyr spread
#' @importFrom Matrix Matrix rowSums colSums
#' @importFrom rlang sym
#' @examples
#' countries_position(
#'     rca = revealed_comparative_advantage_output,
#'     c1 = "country",
#'     p1 = "product",
#'     v1 = "value",
#'     proximity_products = proximity_matrices_output$products_proximity,
#'     p21 = "from",
#'     p22 = "to",
#'     v2 = "value",
#'     pci = complexity_measures_output$product_complexity_index,
#'     p3 = "product",
#'     v3 = "value"
#' )
#' @references
#' For more information on proximity distance, complexity outlook, complexity outlook gain and its
#' applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#' @keywords functions

countries_position <- function(rca = NULL,
                               c1 = "country",
                               p1 = "product",
                               v1 = "value",
                               proximity_products = NULL,
                               p21 = "from",
                               p22 = "to",
                               v2 = "value",
                               pci = NULL,
                               p3 = "product",
                               v3 = "value",
                               tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(rca) %in% c("data.frame") == FALSE)) {
    stop("rca must be a tibble/data.frame")
  }

  if (all(class(proximity_products) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix") == FALSE)) {
    stop("proximity_products must be a tibble/data.frame or matrix")
  }

  if (is.data.frame(rca)) {
    r <- tidyr::spread(rca, !!sym(p1), !!sym(v1))
    r_names <- dplyr::select(r, !!sym(c1)) %>% dplyr::pull()

    rca <- dplyr::select(r, -!!sym(c1)) %>%
      as.matrix()

    rca[is.na(rca)] <- 0
    rownames(rca) <- r_names

    rca <- Matrix::Matrix(rca, sparse = T)
  }

  if (is.data.frame(proximity_products)) {
    p_rows <- dplyr::select(proximity_products, !!sym(p21)) %>% dplyr::pull()
    p_cols <- dplyr::select(proximity_products, !!sym(p22)) %>% dplyr::pull()

    p_cols_rows <- sort(
      unique(c(p_rows, p_cols))
    )

    pp <- expand.grid(
      from = p_cols_rows,
      to = p_cols_rows,
      stringsAsFactors = F
    )

    colnames(pp)  <- c(p21,p22)

    proximity_products <- pp %>%
      dplyr::left_join(proximity_products)

    proximity_products <- tidyr::spread(proximity_products, !!sym(p22), !!sym(v2))
    proximity_products_rows <- dplyr::select(proximity_products, !!sym(p21)) %>% dplyr::pull()

    proximity_products <- proximity_products %>%
      select(-!!sym(p21)) %>%
      as.matrix()

    proximity_products[is.na(proximity_products)] <- 0
    rownames(proximity_products) <- proximity_products_rows

    diag(proximity_products) <- 1
    proximity_products[upper.tri(proximity_products, diag = F)] <- t(proximity_products)[upper.tri(proximity_products, diag = F)]

    proximity_products <- Matrix::Matrix(proximity_products, sparse = T)
  }

  if (is.data.frame(pci)) {
    p <- dplyr::select(pci, !!sym(v3)) %>% dplyr::pull()
    p_names <- dplyr::select(pci, !!sym(p3)) %>% dplyr::pull()

    pci <- p
    names(pci) <- p_names
  }

  dcp <- ((1 - rca) %*% proximity_products) / (Matrix::Matrix(1, nrow = nrow(rca), ncol = ncol(rca)) %*% proximity_products)

  coc <- Matrix::colSums(Matrix::t((1 - dcp) * (1 - rca)) * pci)

  return(
    list(
      proximity_distance = dcp,
      complexity_outlook = coc,
      complexity_outlook_gain = Matrix::t((Matrix::t((1 - dcp) %*% (proximity_products / Matrix::colSums(proximity_products))) * pci) - (Matrix::t(1 - dcp) * pci))
    )
  )
}
