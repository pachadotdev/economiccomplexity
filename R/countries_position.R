#' Countries position
#'
#' @export
#'
#' @param rca matrix or tibble/data.frame (e.g. the
#' output of \code{revealed_comparative_advantage()}).
#' If the input is a matrix it must be a zero/one matrix with countries in rows
#' and products in columns.
#' If the input is a tibble/data.frame it must contain at least three columns
#' with countries, products and values.
#' @param c1 string to indicate the column that contains exporting
#' countries in rca (set to "country" by default)
#' @param p1 string to indicate the column that contains exported products
#' in rca (set to "product" by default)
#' @param v1 string to indicate the column that contains traded values in
#' rca (set to "value" by default)
#' @param pp matrix or tibble/data.frame (e.g. the output of
#' \code{proximity_matrices()}).
#' If the input is a matrix it must be a numeric matrix with products in both
#' rows and columns.
#' If the input is a tibble/data.frame it must contain at least three columns
#' columns with products (twice) and values.
#' @param p21 string to indicate the first column that contains exported
#' products in pp (set to "from" by default)
#' @param p22 string to indicate the second column that contains exported
#' products in pp (set to "to" by default)
#' @param v2 string to indicate the column that contains proximity values in
#' pp (set to "value" by default)
#' @param pci numeric vector or tibble/data.frame with the product complexity
#' index (e.g. the product index from \code{ec_complexity_measures()}).
#' If the input is a vector it must be numeric with optional names.
#' If the input is a tibble/data.frame it must contain at least two columns
#' with products and values.
#' @param p3 string to indicate the column that contains exported products
#' in pp (e.g. "product")
#' @param v3 string to indicate the column that contains proximity values in
#' pp (e.g. "value") from \code{ec_complexity_measures()})
#' @param tbl logical value to use tibble output instead of a matrix
#' output (set to FALSE by default)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select pull
#' @importFrom tidyr spread
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#'
#' @examples
#' ec_countries_position(
#'   rca = ec_output_demo$rca_tbl,
#'   c1 = "country",
#'   p1 = "product",
#'   v1 = "value",
#'   pp = ec_output_demo$proximity_tbl$proximity_p,
#'   p21 = "from",
#'   p22 = "to",
#'   v2 = "value",
#'   pci = ec_output_demo$complexity_measures_tbl$complexity_index_p,
#'   p3 = "product",
#'   v3 = "value",
#'   tbl = TRUE
#' )
#'
#' @references
#' For more information on proximity distance, complexity outlook, complexity
#' outlook gain and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

ec_countries_position <- function(rca = NULL,
                                  c1 = "country",
                                  p1 = "product",
                                  v1 = "value",
                                  pp = NULL,
                                  p21 = "from",
                                  p22 = "to",
                                  v2 = "value",
                                  pci = NULL,
                                  p3 = "product",
                                  v3 = "value",
                                  tbl = FALSE) {
  # sanity checks ----
  if (all(class(rca) %in%
    c(
      "data.frame", "matrix", "dgeMatrix", "dsCMatrix",
      "dgCMatrix"
    ) == FALSE)) {
    stop("rca must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(pp) %in%
    c(
      "data.frame", "matrix", "dgeMatrix", "dsCMatrix",
      "dgCMatrix"
    ) == FALSE)) {
    stop("pp must be a tibble/data.frame or a dense/sparse
         matrix")
  }

  if (all(class(pci) %in% c(
    "data.frame",
    "numeric"
  ) == FALSE)) {
    stop("pci must be a tibble/data.frame or numeric")
  }

  if (is.data.frame(rca)) {
    rca <- tidyr::spread(rca, !!sym(p1), !!sym(v1))

    rca_names <- dplyr::select(rca, !!sym(c1)) %>%
      dplyr::pull()

    rca <- dplyr::select(rca, -!!sym(c1)) %>%
      as.matrix()

    rownames(rca) <- rca_names
  }

  if (is.matrix(rca)) {
    rca[is.na(rca)] <- 0
    rca <- Matrix::Matrix(rca, sparse = TRUE)
  }

  if (is.data.frame(pp)) {
    pp_rows <- dplyr::select(
      pp,
      !!sym(p21)
    ) %>% dplyr::pull()

    pp_cols <- dplyr::select(
      pp,
      !!sym(p22)
    ) %>% dplyr::pull()

    pp_cols_rows <- sort(
      unique(c(pp_rows, pp_cols))
    )

    pp_expand <- expand.grid(
      from = pp_cols_rows,
      to = pp_cols_rows,
      stringsAsFactors = FALSE
    )

    colnames(pp_expand) <- c(p21, p22)

    pp <- pp_expand %>%
      dplyr::left_join(pp)

    pp <- tidyr::spread(
      pp,
      !!sym(p22), !!sym(v2)
    )

    pp_rows <- dplyr::select(
      pp,
      !!sym(p21)
    ) %>% dplyr::pull()

    pp <- pp %>%
      dplyr::select(-!!sym(p21)) %>%
      as.matrix()

    pp[is.na(pp)] <- 0
    rownames(pp) <- pp_rows

    diag(pp) <- 1
    pp[upper.tri(pp, diag = FALSE)] <-
      t(pp)[upper.tri(pp, diag = FALSE)]
  }

  if (is.matrix(pp)) {
    pp <- Matrix::Matrix(pp, sparse = TRUE)
  }

  if (is.data.frame(pci)) {
    pci_values <- dplyr::select(
      pci,
      !!sym(v3)
    ) %>%
      dplyr::pull()

    pci_names <- dplyr::select(
      pci,
      !!sym(p3)
    ) %>%
      dplyr::pull()

    pci <- pci_values
    names(pci) <- pci_names

    pci <- pci[
      sort(names(pci))
    ]
  }

  dcp <- ((1 - rca) %*% pp) /
    (Matrix::Matrix(1, nrow = nrow(rca), ncol = ncol(rca)) %*%
      pp)

  coc <- Matrix::colSums(Matrix::t((1 - dcp) * (1 - rca)) *
    pci)

  cogc <- Matrix::t((Matrix::t((1 - dcp) %*% (pp /
    Matrix::colSums(pp))) * pci) -
    (Matrix::t(1 - dcp) * pci))

  if (tbl == TRUE) {
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
