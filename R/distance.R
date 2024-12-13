#' Distance
#'
#' @description \code{distance()} computes the distance matrix that
#' accounts for the weighted proportions of the products connected to
#' each good that the countries are not exporting.
#'
#' @details The current implementation follows
#' \insertCite{atlas2014}{economiccomplexity} to measure the oppotunities
#' implied by a country's position.
#'
#' @return A matrix.
#'
#' @param balassa_index (Type: matrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param proximity_product (Type: matrix) the output from
#' \code{proximity()}) or an equivalent arrangement.
#'
#' @examples
#' d <- distance(
#'   economiccomplexity_output$balassa_index,
#'   economiccomplexity_output$proximity$proximity_product
#' )
#'
#' # partial view of the distance matrix
#' n <- seq_len(5)
#' d[n, n]
#'
#' @references
#' For more information on this index see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' and the references therein.
#'
#' @keywords functions
#'
#' @export

distance <- function(balassa_index, proximity_product) {
  # sanity checks ----
  if (!(any(class(balassa_index) %in% "matrix") == TRUE)) {
    stop("'balassa_index' must be a matrix")
  }

  distance_(balassa_index, proximity_product)
}
