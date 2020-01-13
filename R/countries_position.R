#' Countries Position
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
#' @importFrom Matrix Matrix rowSums colSums t
#'
#' @examples
#' countries_position(
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

countries_position <- function(balassa_index, proximity_product, complexity_index_product) {
  dcp <- t(t((1 - balassa_index) %*% proximity_product) / rowSums(proximity_product))

  coc <- ((1 - dcp) * (1 - balassa_index)) %*% complexity_index_product

  cogc <- Matrix(0, nrow = nrow(balassa_index), ncol = ncol(balassa_index), sparse = TRUE,
                 dimnames = list(rownames(balassa_index), colnames(balassa_index)))

  for (i in 1:nrow(balassa_index)) {
    p1 <- t((t(proximity_product / colSums(proximity_product)) * (1 - balassa_index[i, ])) %*% complexity_index_product)
    p2 <- dcp[i, ] * complexity_index_product
    cogc[i, ] <- p1 - p2
  }

  return(
    list(
      proximity_distance = dcp,
      complexity_outlook = coc,
      complexity_outlook_gain = cogc
    )
  )
}
