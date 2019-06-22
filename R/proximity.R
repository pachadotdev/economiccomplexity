#' Proximity
#'
#' @export
#' @param revealed_comparative_advantage matrix or tibble/data.frame in long format (e.g. the output of
#' revealed_comparative_advantage()). If it is a matrix it must be a
#' zero/one matrix with countries in the row names and products in the column names.
#' If revealed_comparative_advantage is a tibble/data.frame it must contain the columns.
#' country (character/factor), product (character/factor) and discrete RCA (integer)
#' @param country string to indicate the column that contains exporting countries (default set to "country" that is the
#' output of revealed_comparative_advantage(), applies only if revealed_comparative_advantage is a data.frame)
#' @param product string to indicate the column that contains exported products (default set to "product" that is the
#' output of revealed_comparative_advantage(), applies only if revealed_comparative_advantage is a data.frame)
#' @param value string to indicate the column that contains RCA values (default set to "value" that is the
#' output of revealed_comparative_advantage(), applies only if revealed_comparative_advantage is a data.frame)
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
#'   revealed_comparative_advantage = package_output_demo$revealed_comparative_advantage_matrix,
#'   diversity = package_output_demo$complexity_measures_numeric$diversity,
#'   ubiquity = package_output_demo$complexity_measures_numeric$ubiquity
#' )
#' @references
#' For more information on proximity and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#' @keywords functions

proximity <- function(revealed_comparative_advantage = NULL,
                      country = "country",
                      product = "product",
                      value = "value",
                      diversity = NULL,
                      diversity_country = "country",
                      diversity_value = "value",
                      ubiquity = NULL,
                      ubiquity_product = "product",
                      ubiquity_value = "value",
                      tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(revealed_comparative_advantage) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix", "dgCMatrix") == FALSE)) {
    stop("revealed_comparative_advantage must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(diversity) %in% c("numeric", "data.frame") == FALSE) &
    all(class(ubiquity) %in% c("numeric", "data.frame") == FALSE)) {
    stop("diversity and ubiquity must be numeric or tibble/data.frame")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  # transformations if revealed_comparative_advantage, diversity, ubiquity are data frames ----
  if (is.data.frame(revealed_comparative_advantage)) {
    revealed_comparative_advantage <- tidyr::spread(revealed_comparative_advantage, !!sym(product), !!sym(value))

    revealed_comparative_advantage_rownames <- dplyr::select(revealed_comparative_advantage, !!sym(country)) %>%
      dplyr::pull()

    revealed_comparative_advantage <- dplyr::select(revealed_comparative_advantage, -!!sym(country)) %>%
      as.matrix()

    revealed_comparative_advantage[is.na(revealed_comparative_advantage)] <- 0

    rownames(revealed_comparative_advantage) <- revealed_comparative_advantage_rownames

    revealed_comparative_advantage <- Matrix::Matrix(revealed_comparative_advantage, sparse = T)
    revealed_comparative_advantage <- revealed_comparative_advantage[
      Matrix::rowSums(revealed_comparative_advantage) != 0,
      Matrix::colSums(revealed_comparative_advantage) != 0
    ]
  } else {
    revealed_comparative_advantage <- revealed_comparative_advantage[
      Matrix::rowSums(revealed_comparative_advantage) != 0,
      Matrix::colSums(revealed_comparative_advantage) != 0
    ]
  }

  if (is.data.frame(diversity)) {
    diversity_value <- dplyr::select(diversity, !!sym(diversity_value)) %>% dplyr::pull()
    names(diversity_value) <- dplyr::select(diversity, !!sym(diversity_country)) %>% dplyr::pull()
    diversity <- diversity_value
  }

  if (is.data.frame(ubiquity)) {
    ubiquity_value <- dplyr::select(ubiquity, !!sym(ubiquity_value)) %>% dplyr::pull()
    names(ubiquity_value) <- dplyr::select(ubiquity, !!sym(ubiquity_product)) %>% dplyr::pull()
    ubiquity <- ubiquity_value
  }

  # compute proximity matrices ----
  nc <- nrow(revealed_comparative_advantage)
  xc <- revealed_comparative_advantage %*% Matrix::t(revealed_comparative_advantage)
  yc <- outer(diversity, diversity, pmax)

  np <- ncol(revealed_comparative_advantage)
  xp <- Matrix::t(revealed_comparative_advantage) %*% revealed_comparative_advantage
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
      proximity_countries = cp,
      proximity_products = pp
    )
  )
}
