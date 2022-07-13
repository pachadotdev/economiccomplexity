#' Density
#'
#' @description \code{density()} computes the density matrix that
#' accounts for the weighted proportions of the products connected to
#' each good that the countries are exporting.
#'
#' @details The current implementation follows
#' \insertCite{atlas2014}{economiccomplexity} to measure the oppotunities
#' implied by a country's position.
#'
#' @return A matrix.
#'
#' @param balassa_index (Type: dgCMatrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param proximity_product (Type: dgCMatrix) the output from
#' \code{proximity()}) or an equivalent arrangement.
#'
#' @importFrom Matrix tcrossprod rowSums
#'
#' @examples
#' d <- density(
#'  economiccomplexity_output$balassa_index,
#'  economiccomplexity_output$proximity$proximity_product
#' )
#'
#' # partial view of the density matrix
#' d[1:5,1:5]
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

density <- function(balassa_index, proximity_product) {
  # sanity checks ----
  if (!(any(class(balassa_index) %in% "dgCMatrix") == TRUE)) {
    stop("'balassa_index' must be a dgCMatrix")
  }

  if (!(any(class(proximity_product) %in% "dsCMatrix") == TRUE)) {
    stop("'proximity_product' must be a dgCMatrix")
  }

  return(
    tcrossprod(1 - balassa_index, proximity_product / rowSums(proximity_product))
  )
}
