#' Complexity Outlook
#'
#' @description foo
#'
#' @details bar
#'
#' @return foobar
#'
#' @param balassa_index (Type: matrix or dgCMatrix) foo
#' @param proximity_product (Type: matrix or dgCMatrix) foo.
#' @param complexity_index_product (Type: numeric) foobar.
#'
#' @importFrom Matrix Matrix tcrossprod rowSums colSums t
#'
#' @examples
#' complexity_outlook(
#'  economiccomplexity_output$balassa_index,
#'  economiccomplexity_output$proximity$proximity_product,
#'  economiccomplexity_output$complexity_measures$complexity_index_product
#' )
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

complexity_outlook <- function(balassa_index, proximity_product, complexity_index_product) {
  # sanity checks ----
  if (class(balassa_index) != "dgCMatrix") {
    stop("'balassa_index' must be a dgCMatrix")
  }

  if (class(proximity_product) != "dsCMatrix") {
    stop("'proximity_product' must be a dgCMatrix")
  }

  if (class(complexity_index_product) != "numeric") {
    stop("'complexity_index_product' must be numeric")
  }

  # compute matrices ----
  density <- tcrossprod(1 - balassa_index, proximity_product / rowSums(proximity_product))

  coi <- colSums(t((1 - density) * (1 - balassa_index)) * complexity_index_product)

  cog <- (1 - balassa_index) * tcrossprod((1 - balassa_index),
    t(proximity_product * (complexity_index_product / rowSums(proximity_product))))

  return(
    list(
      complexity_outlook_index = coi,
      complexity_outlook_gain = cog
    )
  )
}
