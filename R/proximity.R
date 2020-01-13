#' Proximity
#'
#' @description TBDs
#'
#' @details TBD
#'
#' @param balassa_index a data frame (e.g. the output from
#' \code{balassa_index()}).
#' @param balassa_sum_source numeric vector or tibble/data.frame containing the
#' Balassa sum for elements of set X (e.g. \code{balassa_sum_source} from
#' \code{complexity_measures()}).
#' @param balassa_sum_target numeric vector or tibble/data.frame containing the
#' Balassa sum for elements of set Y (e.g. \code{balassa_sum_target} from
#' \code{complexity_measures()}).
#' @param compute which proximity to compute. By default is "both" (both
#' proximities) but it can also be "source" or "target".
#'
#' @importFrom Matrix t tril rowSums colSums
#'
#' @examples
#' proximity(
#'   balassa_index = economiccomplexity_output$balassa_index,
#'   balassa_sum_source = economiccomplexity_output$complexity_measures$balassa_sum_source,
#'   balassa_sum_target = economiccomplexity_output$complexity_measures$balassa_sum_target
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

proximity <- function(balassa_index, balassa_sum_source, balassa_sum_target, compute = "both") {
  # sanity checks ----
  if (class(balassa_index) != "dgCMatrix") {
    stop("'balassa_index' must be a dgCMatrix")
  }

  if (class(balassa_sum_source) != "numeric" |
    class(balassa_sum_target) != "numeric") {
    stop("'balassa_sum_source' and 'balassa_sum_target' must be numeric")
  }

  if (is.numeric(balassa_sum_source) & is.null(names(balassa_sum_source))) {
    stop("'balassa_sum_source' cannot have NULL names")
  }

  if (is.numeric(balassa_sum_target) & is.null(names(balassa_sum_target))) {
    stop("'balassa_sum_target' cannot have NULL names")
  }

  if (!any(compute %in% c("both", "source", "target"))) {
    stop("'compute' must be 'both', 'source' or 'target'")
  }

  # compute proximity matrices ----

  if (compute == "both") {
    compute2 <- c("source", "target")
  } else {
    compute2 <- compute
  }

  balassa_index <- balassa_index[rowSums(balassa_index) != 0, colSums(balassa_index) != 0]

  balassa_index <- balassa_index[rownames(balassa_index) %in% names(balassa_sum_source), ]
  balassa_index <- balassa_index[, colnames(balassa_index) %in% names(balassa_sum_target)]

  if (any("source" %in% compute2) == TRUE) {
    prox_x <- (balassa_index %*% t(balassa_index)) / outer(balassa_sum_source, balassa_sum_source, pmax)
    prox_x <- Matrix(prox_x, sparse = T)
  } else {
    prox_x <- NULL
  }

  if (any("target" %in% compute2) == TRUE) {
    prox_y <- (t(balassa_index) %*% balassa_index) / outer(balassa_sum_target, balassa_sum_target, pmax)
    prox_y <- Matrix(prox_y, sparse = T)
  } else {
    prox_y <- NULL
  }

  return(
    list(
      proximity_source = prox_x,
      proximity_target = prox_y
    )
  )
}
