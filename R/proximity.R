#' Proximity
#'
#' @export
#'
#' @param rca matrix or tibble/data.frame in long format (e.g. the output of
#' revealed_comparative_advantage()). If it is a matrix it must be a zero/one
#' matrix with countries in the row names and ps in the column names.
#' If rca is a tibble/data.frame it must contain the columns c (character/
#' factor), p (character/factor) and discrete RCA (integer)
#' @param c string to indicate the column that contains exporting
#' countries (default set to "country" that is the output of
#' revealed_comparative_advantage(), applies only if rca is a data.frame)
#' @param p string to indicate the column that contains exported ps
#' (default set to "product" that is the output of
#' revealed_comparative_advantage(), applies only if rca is a data.frame)
#' @param v string to indicate the column that contains RCA vs (default
#' set to "value" that is the output of revealed_comparative_advantage(),
#' applies only if rca is a data.frame)
#' @param d numeric vector or tibble/data.frame containing diversity
#' measures (e.g. \code{d} from \code{complexity_measures()})
#' @param u numeric vector or tibble/data.frame containing ubiquity
#' measures (e.g. \code{u} from \code{complexity_measures()})
#' @param d_c string to indicate the column that contains
#' countries in "diversity" (default set to "country" that is the output of
#' economic_complexity_measures())
#' @param d_v string to indicate the column that contains
#' values in "diversity" (default set to "value" that is the output of
#' economic_complexity_measures())
#' @param u_p string to indicate the column that contains
#' products in "ubiquity" (default set to "product" that is the output of
#' economic_complexity_measures())
#' @param u_v string to indicate the column that contains
#' values in "ubiquity" (default set to "value" that is the output of
#' economic_complexity_measures())
#' @param tbl when set to TRUE the output will be a tibble instead of a
#' matrix (default set to FALSE)
#' @param compute by default set to "both", it can also be "country" or
#' "product"
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#'
#' @examples
#' ec_proximity(
#'   rca = ec_output_demo$rca_tbl,
#'   d = ec_output_demo$complexity_measures_tbl$diversity,
#'   u = ec_output_demo$complexity_measures_tbl$ubiquity,
#'   tbl = TRUE
#' )
#' @references
#' For more information on proximity and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

ec_proximity <- function(rca = NULL,
                         c = "country",
                         p = "product",
                         v = "value",
                         d = NULL,
                         d_c = "country",
                         d_v = "value",
                         u = NULL,
                         u_p = "product",
                         u_v = "value",
                         tbl = FALSE,
                         compute = "both") {
  # sanity checks ----
  if (all(class(rca) %in% c(
    "data.frame", "matrix", "dgeMatrix", "dsCMatrix",
    "dgCMatrix"
  ) == FALSE)) {
    stop("rca must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(d) %in% c("numeric", "data.frame") == FALSE) &
    all(class(u) %in% c("numeric", "data.frame") == FALSE)) {
    stop("d and u must be numeric or tibble/data.frame")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be logical")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("compute must be both, c or p")
  }

  # transformations if rca, d or u are data frames ----
  if (is.data.frame(rca)) {
    rca <- tidyr::spread(rca, !!sym(p), !!sym(v))

    rca_rownames <- dplyr::select(rca, !!sym(c)) %>%
      dplyr::pull()

    rca <- dplyr::select(rca, -!!sym(c)) %>%
      as.matrix()

    rca[is.na(rca)] <- 0

    rownames(rca) <- rca_rownames

    rca <- Matrix::Matrix(rca, sparse = TRUE)
    rca <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  } else {
    rca <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  }

  if (is.data.frame(d)) {
    d_v <- dplyr::select(d, !!sym(d_v)) %>%
      dplyr::pull()

    names(d_v) <- dplyr::select(
      d,
      !!sym(d_c)
    ) %>%
      dplyr::pull()

    d <- d_v
  }

  if (is.data.frame(u)) {
    u_v <- dplyr::select(u, !!sym(u_v)) %>%
      dplyr::pull()

    names(u_v) <- dplyr::select(
      u,
      !!sym(u_p)
    ) %>%
      dplyr::pull()

    u <- u_v
  }

  # compute proximity matrices ----

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  # remove countries not included in complexity measures
  # (i.e allows to compute after setting atlas= TRUE)
  if (!is.null(d)) {
    rca <- rca[rownames(rca) %in% names(d), ]
  }

  if (!is.null(u)) {
    rca <- rca[, colnames(rca) %in% names(u)]
  }

  if (any("country" %in% compute2) == TRUE) {
    xc <- rca %*% Matrix::t(rca)

    yc <- outer(d, d, pmax)

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

    yp <- outer(u, u, pmax)

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
