#' Proximity
#'
#' @description \code{proximity()} computes two matrices that account for the
#' similarity between pairs of countries and pairs of products.
#'
#' @details The current implementation follows
#' \insertCite{atlas2014}{economiccomplexity} to obtain the likelihood that two
#' products "p1" and "p2" are exported by the same country and, conversely,
#' that two countries "c1" and "c2" export the same product.
#'
#' @return A list of two matrices.
#'
#' @param balassa_index (Type: dgCMatrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param compute (Type: character) the proximity to compute. By default this is
#' \code{"both"} (both proximity matrices) but it can also be \code{"country"}
#' or \code{"product"}.
#'
#' @importFrom Matrix t rowSums colSums crossprod tcrossprod
#'
#' @examples
#' pro <- proximity(economiccomplexity_output$balassa_index)
#'
#' # partial view of proximity matrices
#' pro$proximity_country[1:5,1:5]
#' pro$proximity_product[1:5,1:5]
#'
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

proximity <- function(balassa_index, compute = "both") {
  # sanity checks ----
  if (!(any(class(balassa_index) %in% "dgCMatrix") == TRUE)) {
    stop("'balassa_index' must be a dgCMatrix")
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

  balassa_sum_country <- rowSums(balassa_index)
  balassa_sum_product <- colSums(balassa_index)

  if (any("country" %in% compute2) == TRUE) {
    prox_x <- tcrossprod(balassa_index, balassa_index) / outer(balassa_sum_country, balassa_sum_country, pmax)
    prox_x <- Matrix(prox_x, sparse = TRUE, forceCheck = TRUE)
  } else {
    prox_x <- NULL
  }

  if (any("product" %in% compute2) == TRUE) {
    prox_y <- crossprod(balassa_index, balassa_index) / outer(balassa_sum_product, balassa_sum_product, pmax)
    prox_y <- Matrix(prox_y, sparse = TRUE, forceCheck = TRUE)
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
