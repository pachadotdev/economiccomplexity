#' Proximity Matrices
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
#' @param diversity vector or tibble/data.frame containing diversity measures
#' @param ubiquity vector or tibble/data.frame containing ubiquity measures
#' @param diversity_c string to indicate the column that contains diversity countries (default set to "country" that is the
#' output of economic_complexity_measures())
#' @param diversity_v string to indicate the column that contains diversity values (default set to "value" that is the
#' output of economic_complexity_measures())
#' @param ubiquity_p string to indicate the column that contains diversity products (default set to "product" that is the
#' output of economic_complexity_measures())
#' @param ubiquity_v string to indicate the column that contains ubiquity values (default set to "value" that is the
#' output of economic_complexity_measures())
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom methods as
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom rlang sym
#' @examples
#' proximity <- proximity_matrices(d = world_rca_2017,
#'     diversity = complexity_measures_2017$diversity,
#'     ubiquity = complexity_measures_2017$ubiquity)
#' @keywords functions

proximity_matrices <- function(d = NULL, c = "country", p = "product", v = "value",
                               diversity = NULL, ubiquity = NULL,
                               diversity_c = "country", diversity_v = "value",
                               ubiquity_p = "product", ubiquity_v = "value",
                               tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(d) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix") == FALSE)) {
    stop("d must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(diversity) %in% c("numeric", "data.frame") == FALSE) &
      all(class(ubiquity) %in% c("numeric", "data.frame") == FALSE)) {
    stop("diversity and ubiquity must be numeric or tibble/data.frame")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  # transformations if d, diversity, ubiquity are data frames ----
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

  if (is.data.frame(diversity)) {
    diversity0 <- dplyr::select(diversity, !!sym(diversity_v)) %>% dplyr::pull()
    names(diversity0) <- dplyr::select(diversity, !!sym(diversity_c)) %>% dplyr::pull()

    diversity <- diversity0
  }

  if (is.data.frame(ubiquity)) {
    ubiquity0 <- dplyr::select(ubiquity, !!sym(ubiquity_v)) %>% dplyr::pull()
    names(ubiquity0) <- dplyr::select(ubiquity, !!sym(ubiquity_p)) %>% dplyr::pull()

    ubiquity <- ubiquity0
  }

  # compute proximity matrices ----
  nc <- nrow(m)
  xc <- m %*% Matrix::t(m)
  yc <- outer(diversity, diversity)

  np <- ncol(m)
  xp <- Matrix::t(m) %*% m
  yp <- outer(ubiquity, ubiquity)

  if (tbl_output == FALSE) {
    cp <- as(xc / yc, "dgCMatrix")
    pp <- as(xp / yp, "dgCMatrix")
  }

  # transform proximity matrices to data.frame ----
  if (tbl_output == TRUE) {
    cp <- xc / yc
    pp <- xp / yp

    cp[upper.tri(cp, diag = T)] <- 0

    cp <- as.matrix(cp) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = rownames(cp)) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)

    pp[upper.tri(pp, diag = T)] <- 0

    pp <- as.matrix(pp) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = rownames(pp)) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)
  }

  return(
    list(
      countries_proximity = cp,
      products_proximity = pp
    )
  )
}
