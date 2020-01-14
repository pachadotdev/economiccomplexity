#' Balassa Index
#'
#' @description \code{balassa_index()} computes the Balassa Index for a
#' bipartite relation between countries and products.
#'
#' @details The current implementation follows
#' \insertCite{measuringcomplexity2015}{economiccomplexity} to obtain a metric for
#' specialisation. In the context of international trade, if the Balassa Index
#' for a country-product pair is more than 1, it means that country is
#' specialized in that product.
#'
#' If the input for this function is a data.frame instead of a matrix, the
#' function shall aggregate the data and convert the input to a matrix. You
#' don't need to aggregate the data beforehand if, for example, you pass a
#' data.frame containing origin, destination, product and exports value.
#'
#' @return A matrix with the Balassa Index.
#'
#' @param data (Type: data.frame, matrix or dgCMatrix) a dataset such as \code{galactic_federation}
#' or any arrangement.
#' @param discrete (Type: logical) whether converting the Balassa Index to
#' discrete (0/1) values. Anything below the specified cutoff is converted to 0
#' and 1 otherwise. By default this is set to \code{TRUE}.
#' @param cutoff (Type: numeric) the cutoff to use for discretization.
#' By default this is set to \code{1}.
#' @param country (Type: character) the column with the countries.
#' By default this is set to \code{"country"}. Applies only if the input is a data.frame.
#' @param product (Type: character) the column with the products.
#' By default this is set to \code{"product"}. Applies only if the input is a data.frame.
#' @param value  (Type: character) the column with the metric for
#' country-product pairs.
#' By default this is set to \code{"value"}. Applies only if the input is a data.frame.
#'
#' @importFrom Matrix Matrix rowSums colSums t
#'
#' @examples
#' balassa_index(
#'   data = galactic_federation,
#'   country = "planet",
#'   product = "product",
#'   value = "export_value"
#' )
#'
#' @references
#' For more information see:
#'
#' \insertRef{measuringcomplexity2015}{economiccomplexity}
#'
#' and the references therein.
#'
#' @keywords functions
#'
#' @export

balassa_index <- function(data, discrete = TRUE, cutoff = 1,
                          country = "country", product = "product", value = "value") {
  # sanity checks ----
  if (!any(class(data) %in% c("data.frame", "matrix", "dgCMatrix"))) {
    stop("'data' must be a data.frame, matrix or dgCMatrix")
  }

  if (!is.character(country) | !is.character(product) | !is.character(value)) {
    stop("'country', 'product' and 'value' must be of type character")
  }

  if (!is.logical(discrete)) {
    stop("'discrete' must be TRUE or FALSE")
  }

  if (!is.numeric(cutoff)) {
    stop("'cutoff' must be numeric")
  }

  # convert data.frame / matrix to dgCMatrix ----
  if (any(class(data) %in% "data.frame")) {
    data <- country_product_aggregation(data, country, product, value)
    data <- dataframe_to_matrix(data, country, product, value)
  }

  if (class(data) == "matrix") {
    data <- Matrix(data, sparse = TRUE)
  }

  # compute index ----
  data <- t(t(data / rowSums(data)) / (colSums(data) / sum(data)))

  if (discrete == TRUE) {
    data[data < cutoff] <- 0
    data[data >= cutoff] <- 1
  }

  return(data)
}
