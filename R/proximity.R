#' Proximity
#'
#' @export
#' @param m matrix
#' @param kc vector
#' @param kp vector
#' @param output matrix or tibble
#' @importFrom methods as
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate
#' @importFrom tidyr gather
#' @importFrom rlang sym
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
#' proximity <- proximity(m, kc = kc0, kp = kp0, output = "matrix")
#' @keywords functions

proximity <- function(m, kc, kp, output = "matrix") {
  if (!output %in% c("matrix", "tibble")) {
    stop()
  }

  nc <- nrow(m)
  xc <- m %*% Matrix::t(m)
  yc <- outer(kc, kc)

  np <- ncol(m)
  xp <- Matrix::t(m) %*% m
  yp <- outer(kp, kp)

  if (output == "matrix") {
    pc <- as(xc / yc, "dgCMatrix")
    pp <- as(xp / yp, "dgCMatrix")
  }

  if (output == "tibble") {
    pc <- xc / yc
    pp <- xp / yp

    pc[upper.tri(pc, diag = T)] <- 0

    pc <- as.matrix(pc) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = rownames(pc)) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)

    pp[upper.tri(pp, diag = T)] <- 0

    pp <- as.matrix(pp) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = rownames(pp)) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)
  }

  proximity <- list(proximity_countries = pc, proximity_products = pp)
  return(proximity)
}
