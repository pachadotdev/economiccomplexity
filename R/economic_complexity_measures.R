#' Economic Complexity Measures
#'
#' @export
#' @param d matrix or tibble/data.frame in long format, if d is a tibble/data.frame it must contain the columns
#' country (character/factor), product (character/factor) and discrete RCA (integer), if it is a matrix it must be a
#' zero/one matrix with countries in the row names and products in the column names
#' @param c string to indicate the column that contains exporting countries (default set to "country" that is the
#' output of revealed_comparative_advantage())
#' @param p string to indicate the column that contains exported products (default set to "product" that is the
#' output of revealed_comparative_advantage())
#' @param v string to indicate the column that contains RCA values (default set to "value" that is the
#' output of revealed_comparative_advantage())
#' @param use_eigenvalues when set to TRUE the function will use eigenvalues as a limit case of the reflections
#' methods (default set to FALSE)
#' @param iterations number of iterations to use in the reflections method (default set to 20)
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate arrange pull
#' @importFrom tidyr gather spread
#' @importFrom tibble tibble as_tibble enframe
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym
#' @examples
#' complexity_measures <- economic_complexity_measures(world_rca_2017)
#' @keywords functions

economic_complexity_measures <- function(d = NULL, c = "country", p = "product", v = "value",
                                         use_eigenvalues = FALSE, iterations = 20, tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(d) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix") == FALSE)) {
    stop("d must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!is.logical(use_eigenvalues)) {
    stop("use_eigenvalues must be logical")
  }

  if (is.integer(iterations) & !iterations >= 2) {
    stop("iterations must be integer and greater or equal to 2")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  # convert data.frame input to matrix ----
  if (is.data.frame(d)) {
    m <- tidyr::spread(d, !!sym(p), !!sym(v))
    m_rownames <- dplyr::select(m, !!sym(c)) %>% dplyr::pull()

    m <- dplyr::select(m, -!!sym(c)) %>% as.matrix()
    m[is.na(m)] <- 0
    rownames(m) <- m_rownames

    m <- Matrix::Matrix(m, sparse = T)
    m <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]
  } else {
    m <- d[Matrix::rowSums(d) != 0, Matrix::colSums(d) != 0]
  }

  # compute complexity measures ----
  # diversity (kc0) and ubiquity (kp0), following the Atlas notation
  kc0 <- Matrix::rowSums(m)
  kp0 <- Matrix::colSums(m)

  if (use_eigenvalues == FALSE) {
    # create empty matrices
    kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = iterations, sparse = T)
    kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = iterations, sparse = T)

    # fill the first columns with kc0 and kp0 to start iterating
    kc[, 1] <- kc0
    kp[, 1] <- kp0

    # compute cols 2 to "no. of iterations" by iterating from col 1
    for (j in 2:ncol(kc)) {
      kc[, j] <- (m %*% kp[, (j - 1)]) * (1 / kc0)
      kp[, j] <- (Matrix::t(m) %*% kc[, (j - 1)]) * (1 / kp0)
    }

    # eci is of odd order and normalized as in the Atlas
    eci <- (kc[, iterations - 1] - base::mean(kc[, iterations - 1])) /
      stats::sd(kc[, iterations - 1])

    # pci is of even order and normalized as in the Atlas
    pci <- (kp[, iterations] - base::mean(kp[, iterations])) /
      stats::sd(kp[, iterations])
  }

  if (use_eigenvalues == TRUE) {
    eci <- eigen((m %*% (Matrix::t(m) * (1 / kp0))) * (1 / kc0))
    eci <- Re(eci$vectors[, 2])
    eci <- (eci - base::mean(eci)) / stats::sd(eci)

    pci <- eigen((Matrix::t(m) %*% (m * (1 / kc0))) * (1 / kp0))
    pci <- Re(pci$vectors[, 2])
    pci <- (pci - base::mean(pci)) /  stats::sd(pci)
  }

  names(eci) <- rownames(m)
  names(pci) <- colnames(m)

  if (tbl_output == TRUE) {
    eci <- tibble::tibble(value = eci) %>%
      dplyr::mutate(country = names(eci)) %>%
      dplyr::select(!!sym("country"), !!sym("value")) %>%
      dplyr::arrange(-!!sym("value"))

    pci <- tibble::tibble(value = pci) %>%
      dplyr::mutate(product = names(pci)) %>%
      dplyr::select(!!sym("product"), !!sym("value")) %>%
      dplyr::arrange(-!!sym("value"))

    kc0 <- tibble::enframe(kc0) %>%
      dplyr::rename(country = !!sym("name"))

    kp0 <- tibble::enframe(kp0) %>%
      dplyr::rename(product = !!sym("name"))

    m <- as.matrix(m) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(country = rownames(m)) %>%
      tidyr::gather(!!sym("product"), !!sym("value"), -!!sym("country"))
  }

  return(
    list(
      economic_complexity_index = eci,
      product_complexity_index = pci,
      diversity = kc0,
      ubiquity = kp0
    )
  )
}
