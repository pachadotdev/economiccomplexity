#' Proximity
#'
#' @export
#' @param rca matrix or tibble/data.frame in long format (e.g. the output of
#' revealed_comparative_advantage()). If it is a matrix it must be a zero/one
#' matrix with countries in the row names and products in the column names.
#' If rca is a tibble/data.frame it must contain the columns country (character/
#' factor), product (character/factor) and discrete RCA (integer)
#' @param country string to indicate the column that contains exporting
#' countries (default set to "country" that is the output of
#' revealed_comparative_advantage(), applies only if rca is a data.frame)
#' @param product string to indicate the column that contains exported products
#' (default set to "product" that is the output of
#' revealed_comparative_advantage(), applies only if rca is a data.frame)
#' @param value string to indicate the column that contains RCA values (default
#' set to "value" that is the output of revealed_comparative_advantage(),
#' applies only if rca is a data.frame)
#' @param diversity numeric vector or tibble/data.frame containing diversity
#' measures (e.g. \code{diversity} from \code{complexity_measures()})
#' @param ubiquity numeric vector or tibble/data.frame containing ubiquity
#' measures (e.g. \code{ubiquity} from \code{complexity_measures()})
#' @param diversity_country string to indicate the column that contains
#' diversity countries (default set to "country" that is the output of
#' economic_complexity_measures())
#' @param diversity_value string to indicate the column that contains diversity
#' values (default set to "value" that is the output of
#' economic_complexity_measures())
#' @param ubiquity_product string to indicate the column that contains diversity
#' products (default set to "product" that is the output of
#' economic_complexity_measures())
#' @param ubiquity_value string to indicate the column that contains ubiquity
#' values (default set to "value" that is the output of
#' economic_complexity_measures())
#' @param tbl_output when set to TRUE the output will be a tibble instead of a
#' matrix (default set to FALSE)
#' @param compute by default set to "both", it can also be "country" or
#' "product"
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#' @examples
#' proximity(
#'  rca = package_output_demo$revealed_comparative_advantage_matrix,
#'  diversity = package_output_demo$complexity_measures_numeric$diversity,
#'  ubiquity = package_output_demo$complexity_measures_numeric$ubiquity,
#'  tbl_output = TRUE
#' )
#'
#' @references
#' For more information on proximity and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

proximity <- function(rca = NULL,
                      country = "country",
                      product = "product",
                      value = "value",
                      diversity = NULL,
                      diversity_country = "country",
                      diversity_value = "value",
                      ubiquity = NULL,
                      ubiquity_product = "product",
                      ubiquity_value = "value",
                      tbl_output = FALSE,
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

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("compute must be both, country or product")
  }

  # transformations if rca, diversity or ubiquity are data frames ----
  if (is.data.frame(rca)) {
    rca <- tidyr::spread(rca, !!sym(product), !!sym(value))

    rca_rownames <- dplyr::select(rca, !!sym(country)) %>%
      dplyr::pull()

    rca <- dplyr::select(rca, -!!sym(country)) %>%
      as.matrix()

    rca[is.na(rca)] <- 0

    rownames(rca) <- rca_rownames

    rca <- Matrix::Matrix(rca, sparse = TRUE)
    rca <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  } else {
    rca <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  }

  if (is.data.frame(diversity)) {
    diversity_value <- dplyr::select(diversity, !!sym(diversity_value)) %>%
      dplyr::pull()

    names(diversity_value) <- dplyr::select(diversity,
      !!sym(diversity_country)) %>%
      dplyr::pull()

    diversity <- diversity_value
  }

  if (is.data.frame(ubiquity)) {
    ubiquity_value <- dplyr::select(ubiquity, !!sym(ubiquity_value)) %>%
      dplyr::pull()

    names(ubiquity_value) <- dplyr::select(ubiquity,
      !!sym(ubiquity_product)) %>%
      dplyr::pull()

    ubiquity <- ubiquity_value
  }

  # compute proximity matrices ----
  if (compute == "both") {
    compute2 <- c("country","product")
  } else {
    compute2 <- compute
  }

  if (any("country" %in% compute2) == TRUE) {
    xc <- rca %*% Matrix::t(rca)
    yc <- outer(diversity, diversity, pmax)

    if (tbl_output == FALSE) {
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

    if (tbl_output == FALSE) {
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
      proximity_countries = cp,
      proximity_products = pp
    )
  )
}
