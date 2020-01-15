#' Proximity
#'
#' @description \code{proximity()} computes two matrices obtained after the
#' Balassa Index that account for the similarity between pairs of countries
#' and pairs of products.
#'
#' @details The current implementation follows
#' \insertCite{atlas2014}{economiccomplexity} to obtain the likelihood that two
#' products "p1" and "p2" are exported by the same country and, conversely,
#' that two countries "c1" and "c2" export the same product.
#'
#' @return a list of two matrices
#'
#' @param balassa_index a data frame (e.g. the output from
#' \code{balassa_index()}).
#' @param balassa_sum_country numeric vector or tibble/data.frame containing the
#' Balassa sum for elements of set X (e.g. \code{balassa_sum_country} from
#' \code{complexity_measures()}).
#' @param balassa_sum_product numeric vector or tibble/data.frame containing the
#' Balassa sum for elements of set Y (e.g. \code{balassa_sum_product} from
#' \code{complexity_measures()}).
#' @param compute which proximity to compute. By default is "both" (both
#' proximities) but it can also be "country" or "product".
#'
#' @importFrom Matrix t rowSums colSums crossprod tcrossprod
#'
#' @examples
#' proximity(
#'   balassa_index = economiccomplexity_output$balassa_index,
#'   balassa_sum_country = economiccomplexity_output$complexity_measures$balassa_sum_country,
#'   balassa_sum_product = economiccomplexity_output$complexity_measures$balassa_sum_product
#' )
#' @references
#' For more information see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' and the references therein.
#'
#' @keywords functions
#'
#' @export

proximity <- function(balassa_index, balassa_sum_country, balassa_sum_product, compute = "both") {
  # sanity checks ----
  if (class(balassa_index) != "dgCMatrix") {
    stop("'balassa_index' must be a dgCMatrix")
  }

  if (class(balassa_sum_country) != "numeric" |
    class(balassa_sum_product) != "numeric") {
    stop("'balassa_sum_country' and 'balassa_sum_product' must be numeric")
  }

  if (is.numeric(balassa_sum_country) & is.null(names(balassa_sum_country))) {
    stop("'balassa_sum_country' cannot have NULL names")
  }

  if (is.numeric(balassa_sum_product) & is.null(names(balassa_sum_product))) {
    stop("'balassa_sum_product' cannot have NULL names")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("'compute' must be 'both', 'country' or 'product'")
  }

  # compute proximity matrices ----

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  balassa_index <- balassa_index[rowSums(balassa_index) != 0, colSums(balassa_index) != 0]

  balassa_index <- balassa_index[rownames(balassa_index) %in% names(balassa_sum_country), ]
  balassa_index <- balassa_index[, colnames(balassa_index) %in% names(balassa_sum_product)]

  if (any("country" %in% compute2) == TRUE) {
    prox_x <- tcrossprod(balassa_index, balassa_index) / outer(balassa_sum_country, balassa_sum_country, pmax)
    prox_x <- Matrix(prox_x, sparse = TRUE)
  } else {
    prox_x <- NULL
  }

  if (any("product" %in% compute2) == TRUE) {
    prox_y <- crossprod(balassa_index, balassa_index) / outer(balassa_sum_product, balassa_sum_product, pmax)
    prox_y <- Matrix(prox_y, sparse = TRUE)
  } else {
    prox_y <- NULL
  }

  return(
    list(
      proximity_country = prox_x,
      proximity_product = prox_y
    )
  )
}
