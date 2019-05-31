#' Proximity
#'
#' @export
#' @param m matrix
#' @param kc vector
#' @param kp vector
#' @importFrom methods as
#' @examples
#' rca <- rca(d = world_trade_1980, c = "reporter_iso",
#'     p = "product_code", x = "export_value_usd")
#'
#' indices <- indices(rca, method = "reflections", maxiter = 20,
#'     output = "matrix")
#'
#' m <- indices$m
#' kc0 <- indices$kc0
#' kp0 <- indices$kp0
#'
#' proximity <- proximity(m, kc = kc0, kp = kp0)
#' @keywords functions

proximity <- function(m, kc, kp) {
  nc <- nrow(m)
  xc <- m %*% Matrix::t(m)
  yc <- economiccomplexity::pairwise_max(nc,kc)

  np <- ncol(m)
  xp <- Matrix::t(m) %*% m
  yp <- economiccomplexity::pairwise_max(np,kp)

  proximity <- list(proximity_countries = as(xc / yc, "dgCMatrix"),
                    proximity_products = as(xp / yp, "dgCMatrix"))

  return(proximity)
}
