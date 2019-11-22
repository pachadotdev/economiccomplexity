#' @title Proximity
#'
#' @description \code{proximity} computes proximity
#'
#' @description Given a \eqn{C\times P} matrix (C for "countries"
#' and P for "products") with RCA values, a C lenght vector with diversity
#' values and a P length vector with ubiquity values or equivalent data frames,
#' this function implements the equations:
#' \deqn{\text{(Product proximity)}\: \phi_{pp'} = \frac{\sum_c R_{cp}
#' R_{cp'}}{\max(k_{p}^{(0)}, k_{p'}^{(0)})}}
#' \deqn{\text{(Country proximity}\:\lambda_{cc'} = \frac{\sum_p R_{c,p}
#' R_{c,p'}}{\max(k_{c}^{(0)}, k_{c'}^{(0)})}}
#'
#' @param rca matrix or data.frame with RCA values
#' @param diversity matrix or data.frame with diversity values
#' @param ubiquity matrix or data.frame with ubiquity values
#' @param country_r column containing countries (applies only if d is a
#' data.frame)
#' @param product_r column containing products (applies only if d is a
#' data.frame)
#' @param value_r column containing traded values (applies only if d is a
#' data.frame)
#' @param country_d column containing countries (applies only if d is a
#' data.frame)
#' @param value_d column containing values (applies only if d is a
#' data.frame)
#' @param product_u column containing products (applies only if d is a
#' data.frame)
#' @param value_u column containing values (applies only if d is a
#' data.frame)
#' @param tbl TRUE (default) returns a data.frame and FALSE returns a matrix
#' @param compute "country", "product" or "both" (default) matrices
#'
#' @references
#' For more information on proximity and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' and the references therein.
#'
#' @examples
#' proximity(
#'   rca = ec_output_demo$rca_tbl,
#'   d = ec_output_demo$complexity_measures_tbl$diversity,
#'   u = ec_output_demo$complexity_measures_tbl$ubiquity
#' )
#'
#' @return A list with two data frames or matrices.
#'
#' @seealso \code{\link[economiccomplexity]{rca}},
#' \code{\link[economiccomplexity]{complexity}}
#'
#' @keywords functions
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#'
#' @export

proximity <- function(rca,
                      diversity,
                      ubiquity,
                      country_r = "country",
                      product_r = "product",
                      value_r = "value",
                      country_d = "country",
                      value_d = "value",
                      product_u = "product",
                      value_u = "value",
                      tbl = TRUE,
                      compute = "both") {
  # sanity checks ----
  if (all(class(rca) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
    "dgCMatrix") == FALSE)) {
    stop("rca must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(diversity) %in% c("numeric", "data.frame") == FALSE) &
    all(class(ubiquity) %in% c("numeric", "data.frame") == FALSE)) {
    stop("diversity and ubiquity must be numeric or tibble/data.frame")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be logical")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("compute must be both, country or product")
  }

  # transformations if rca, d or u are data frames ----
  if (is.data.frame(rca)) {
    rca <- tidyr::spread(rca, !!sym(product_r), !!sym(value_r))

    rca_rownames <- dplyr::select(rca, !!sym(country_r)) %>%
      dplyr::pull()

    rca <- dplyr::select(rca, -!!sym(country_r)) %>%
      as.matrix()

    rca[is.na(rca)] <- 0

    rownames(rca) <- rca_rownames

    rca <- Matrix::Matrix(rca, sparse = TRUE)
    rca <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  } else {
    rca <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  }

  if (is.data.frame(diversity)) {
    value_d <- dplyr::select(diversity, !!sym(value_d)) %>%
      dplyr::pull()

    names(value_d) <- dplyr::select(diversity, !!sym(country_d)) %>%
      dplyr::pull()

    diversity <- value_d
  }

  if (is.data.frame(ubiquity)) {
    value_u <- dplyr::select(ubiquity, !!sym(value_u)) %>%
      dplyr::pull()

    names(value_u) <- dplyr::select(ubiquity, !!sym(product_u)) %>%
      dplyr::pull()

    ubiquity <- value_u
  }

  # compute proximity matrices ----

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  # remove countries not included in complexity measures
  # (i.e allows to compute after setting atlas= TRUE)
  if (!is.null(diversity)) {
    rca <- rca[rownames(rca) %in% names(diversity), ]
  }

  if (!is.null(ubiquity)) {
    rca <- rca[, colnames(rca) %in% names(ubiquity)]
  }

  if (any("country" %in% compute2) == TRUE) {
    xc <- rca %*% Matrix::t(rca)

    yc <- outer(diversity, diversity, pmax)

    if (tbl == FALSE) {
      cp <- Matrix::Matrix(xc / yc, sparse = TRUE)
    } else {
      cp <- xc / yc

      cp[upper.tri(cp, diag = TRUE)] <- 0

      cp <- as.matrix(cp) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = rownames(cp)) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }
  } else {
    cp <- NULL
  }

  if (any("product" %in% compute2) == TRUE) {
    xp <- Matrix::t(rca) %*% rca

    yp <- outer(ubiquity, ubiquity, pmax)

    if (tbl == FALSE) {
      pp <- Matrix::Matrix(xp / yp, sparse = TRUE)
    } else {
      pp <- xp / yp

      pp[upper.tri(pp, diag = TRUE)] <- 0

      pp <- as.matrix(pp) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = rownames(pp)) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }
  } else {
    pp <- NULL
  }

  return(
    list(
      proximity_c = cp,
      proximity_p = pp
    )
  )
}
