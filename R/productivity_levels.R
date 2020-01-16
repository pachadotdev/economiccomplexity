#' Productivity Levels
#'
#' @description \code{productivity_levels()} computes EXPY and PRODY measures.
#'
#' @details The current implementation follows
#' \insertCite{exportmatters2005}{economiccomplexity} to obtain the
#' productivity and income levels associated to exports.
#'
#' @return A list of two named numeric vectors.
#'
#' @param data_exp (Type: data.frame, matrix or dgCMatrix) a dataset containing
#' countries, products and exported values.
#' @param data_gdp (Type: data.frame, matrix or dgCMatrix) a dataset
#' containing countries and per-capita GDP values.
#' @param country (Type: character) the column with the countries.
#' By default this is set to \code{"country"}. Used only if the input is a data.frame.
#' @param product (Type: character) the column with the products.
#' By default this is set to \code{"product"}. Used only if the input is a data.frame.
#' @param value  (Type: character) the column with the metric for
#' country-product pairs.
#' By default this is set to \code{"value"}. Used only if the input is a data.frame.
#'
#' @importFrom Matrix Matrix rowSums colSums t crossprod
#' @importFrom stats setNames
#'
#' @examples
#' data_exp <- galactic_federation
#'
#' # Since I don't have a gdp per capita dataset for the Galactic Federation,
#' # I'll create simulated data
#' set.seed(1810)
#' data_gdp <- setNames(
#'  rnorm(1:nrow(galactic_federation), 1000, 200),
#'  rownames(galactic_federation)
#' )
#'
#' pl <- productivity_levels(data_exp, data_gdp)
#'
#' @references
#' For more information on prody and its applications see:
#'
#' \insertRef{exportmatters2005}{economiccomplexity}
#'
#' @keywords functions
#'
#' @export

productivity_levels <- function(data_exp, data_gdp,
                                country = "country", product = "product", value = "value") {
  # sanity checks ----
  if (all(class(data_exp) %in% c("data.frame", "matrix", "dgCMatrix") == FALSE)) {
    stop("'data_exp' must be a data.frame, matrix or dgCMatrix")
  }

  if (all(class(data_gdp) %in% c("data.frame", "numeric") == FALSE)) {
    stop("'data_gdp' must be a data.frame or numeric")
  }

  # tidy input data data_exp ----
  if (any(class(data_exp) %in% "data.frame")) {
    data_exp <- country_product_aggregation(data_exp, country, product, value)
    data_exp <- dataframe_to_matrix(data_exp, country, product, value)
  }

  if (class(data_exp) == "matrix") {
    data_exp <- Matrix(data_exp, sparse = TRUE)
  }

  # tidy input data data_gdp ----
  if (any(class(data_gdp) %in% "data.frame")) {
    data_gdp <- country_aggregation(data_gdp, country, value)
    data_gdp <- setNames(as.numeric(data_gdp$value), data_gdp$country)
  }

  if (class(data_gdp) == "matrix") {
    data_gdp <- Matrix(data_gdp, sparse = TRUE)
  }

  intersect_country_1 <- rownames(data_exp) %in% names(data_gdp)
  intersect_country_1 <- intersect_country_1[intersect_country_1 == TRUE]

  intersect_country_2 <- names(data_gdp) %in% rownames(data_exp)
  intersect_country_2 <- intersect_country_2[intersect_country_2 == TRUE]

  if (length(intersect_country_1) != length(intersect_country_2)) {
    warning("'data_exp' and 'data_gdp' don\'t have the same countries, some elements will be dropped")
  }

  data_exp <- data_exp[rownames(data_exp) %in% names(data_gdp), ]
  data_gdp <- data_gdp[names(data_gdp) %in% rownames(data_exp)]

  p1 <- data_exp / rowSums(data_exp)
  p2 <- colSums(p1)

  prody <- crossprod(p1, data_gdp) / p2
  expy <- p1 %*% prody

  prody <- setNames(as.numeric(prody), rownames(prody))
  expy <- setNames(as.numeric(expy), rownames(expy))

  return(
    list(
      productivity_level_country = expy,
      productivity_level_product = prody
    )
  )
}
