#' Proximity
#'
#' @export
#' @param rca matrix or tibble/data.frame in long format (e.g. the output of
#' revealed_comparative_advantage()). If it is a matrix it must be a
#' zero/one matrix with countries in the row names and products in the column names.
#' If rca is a tibble/data.frame it must contain the columns.
#' country (character/factor), product (character/factor) and discrete RCA (integer)
#' @param country string to indicate the column that contains exporting countries (default set to "country" that is the
#' output of revealed_comparative_advantage(), applies only if rca is a data.frame)
#' @param product string to indicate the column that contains exported products (default set to "product" that is the
#' output of revealed_comparative_advantage(), applies only if rca is a data.frame)
#' @param value string to indicate the column that contains RCA values (default set to "value" that is the
#' output of revealed_comparative_advantage(), applies only if rca is a data.frame)
#' @param diversity numeric vector or tibble/data.frame containing diversity measures (e.g. \code{diversity}
#' from \code{complexity_measures()})
#' @param ubiquity numeric vector or tibble/data.frame containing ubiquity measures (e.g. \code{ubiquity}
#' from \code{complexity_measures()})
#' @param diversity_country string to indicate the column that contains diversity countries (default set to "country" that is the
#' output of economic_complexity_measures())
#' @param diversity_value string to indicate the column that contains diversity values (default set to "value" that is the
#' output of economic_complexity_measures())
#' @param ubiquity_product string to indicate the column that contains diversity products (default set to "product" that is the
#' output of economic_complexity_measures())
#' @param ubiquity_value string to indicate the column that contains ubiquity values (default set to "value" that is the
#' output of economic_complexity_measures())
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#' @examples
#' pro <- proximity(
#'   rca = rca_t,
#'   diversity = cm_t$diversity,
#'   ubiquity = cm_t$ubiquity
#' )
#' @references
#' For more information on proximity and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#' @keywords functions

proximity <- function(rca = NULL,
                      country = "country",
                      product = "product",
                      value = "value",
                      diversity = NULL,
                      ubiquity = NULL,
                      diversity_country = "country",
                      diversity_value = "value",
                      ubiquity_product = "product",
                      ubiquity_value = "value",
                      tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(rca) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix", "dgCMatrix") == FALSE)) {
    stop("rca must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(diversity) %in% c("numeric", "data.frame") == FALSE) &
    all(class(ubiquity) %in% c("numeric", "data.frame") == FALSE)) {
    stop("diversity and ubiquity must be numeric or tibble/data.frame")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  # transformations if rca, diversity, ubiquity are data frames ----
  if (is.data.frame(rca)) {
    m <- tidyr::spread(rca, !!sym(product), !!sym(value))
    m_rownames <- dplyr::select(m, !!sym(country)) %>% dplyr::pull()

    m <- dplyr::select(m, -!!sym(country)) %>% as.matrix()
    m[is.na(m)] <- 0
    rownames(m) <- m_rownames

    m <- Matrix::Matrix(m, sparse = T)
    m <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]
  } else {
    m <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  }

  if (is.data.frame(diversity)) {
    diversity0 <- dplyr::select(diversity, !!sym(diversity_value)) %>% dplyr::pull()
    names(diversity0) <- dplyr::select(diversity, !!sym(diversity_country)) %>% dplyr::pull()

    diversity <- diversity0
  }

  if (is.data.frame(ubiquity)) {
    ubiquity0 <- dplyr::select(ubiquity, !!sym(ubiquity_value)) %>% dplyr::pull()
    names(ubiquity0) <- dplyr::select(ubiquity, !!sym(ubiquity_product)) %>% dplyr::pull()

    ubiquity <- ubiquity0
  }

  # compute proximity matrices ----
  nc <- nrow(m)
  xc <- m %*% Matrix::t(m)
  yc <- outer(diversity, diversity, pmax)

  np <- ncol(m)
  xp <- Matrix::t(m) %*% m
  yp <- outer(ubiquity, ubiquity, pmax)

  if (tbl_output == FALSE) {
    cp <- Matrix::Matrix(xc / yc, sparse = T)
    pp <- Matrix::Matrix(xp / yp, sparse = T)
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
