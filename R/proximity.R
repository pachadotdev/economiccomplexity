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
#' @param balassa_index (Type: matrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param compute (Type: character) the proximity to compute. By default this is
#' \code{"both"} (both proximity matrices) but it can also be \code{"country"}
#' or \code{"product"}.
#'
#' @examples
#' pro <- proximity(economiccomplexity_output$balassa_index)
#'
#' # partial view of proximity matrices
#' n <- seq_len(5)
#' pro$proximity_country[n, n]
#' pro$proximity_product[n, n]
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
  if (!(any(class(balassa_index) %in% "matrix") == TRUE)) {
    stop("'balassa_index' must be a matrix")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("'compute' must be 'both', 'country' or 'product'")
  }

  # compute proximity matrices ----
  balassa_index <- balassa_index[rowSums(balassa_index) != 0,
    colSums(balassa_index) != 0]

  proximity_(balassa_index, compute)
}
