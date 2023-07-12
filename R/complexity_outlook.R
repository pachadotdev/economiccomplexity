#' Complexity Outlook
#'
#' @description \code{complexity_outlook()} computes the Complexity Outlook
#' Index and the Complexity Outlook Gain.
#'
#' @details The current implementation follows
#' \insertCite{atlas2014}{economiccomplexity} to measure unexploited
#' export oppportunities.
#'
#' @return A list of two named numeric vectors.
#'
#' @param balassa_index (Type: dgCMatrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param proximity_product (Type: dgCMatrix) the output from
#' \code{proximity()}) or an equivalent arrangement.
#' @param complexity_index_product (Type: numeric) the output from
#' \code{complexity_measures()}) or an equivalent arrangement.
#'
#' @importFrom Matrix tcrossprod rowSums colSums t
#'
#' @examples
#' co <- complexity_outlook(
#'   economiccomplexity_output$balassa_index,
#'   economiccomplexity_output$proximity$proximity_product,
#'   economiccomplexity_output$complexity_measures$complexity_index_product
#' )
#'
#' # partial view of complexity outlook
#' co$complexity_outlook_index[1:5]
#' co$complexity_outlook_gain[1:5, 1:5]
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
  if (!(any(class(balassa_index) %in% "dgCMatrix") == TRUE)) {
    stop("'balassa_index' must be a dgCMatrix")
  }

  if (!(any(class(proximity_product) %in% "dsCMatrix") == TRUE)) {
    stop("'proximity_product' must be a dgCMatrix")
  }

  if (!(any(class(complexity_index_product) %in% "numeric") == TRUE)) {
    stop("'complexity_index_product' must be numeric")
  }

  # compute matrices ----
  dist <- distance(balassa_index, proximity_product)

  coi <- colSums(t((1 - dist) * (1 - balassa_index)) * complexity_index_product)

  cog <- (1 - balassa_index) * tcrossprod(
    (1 - balassa_index),
    t(proximity_product * (complexity_index_product / rowSums(proximity_product)))
  )

  return(
    list(
      complexity_outlook_index = coi,
      complexity_outlook_gain = cog
    )
  )
}
