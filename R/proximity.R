#' Proximity
#'
#' @export
#' @param m matrix
#' @param kc vector
#' @param kp vector
#' @param output matrix or tibble
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

proximity <- function(m, kc, kp, output) {
  nc <- nrow(m)
  xc <- m %*% Matrix::t(m)
  yc <- outer(kc, kc)

  np <- ncol(m)
  xp <- Matrix::t(m) %*% m
  yp <- outer(kp, kp)

  if (output == "matrix") {
    proximity_countries <- xc / yc
    proximity_products <- xp / yp
  }

  if (output == "tibble") {
    proximity_countries <- xc / yc
    proximity_products <- xp / yp

    proximity_countries[upper.tri(proximity_countries, diag = T)] <- NA
    row_names <- rownames(proximity_countries)

    proximity_countries <- as.matrix(proximity_countries) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = row_names) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!is.na(!!sym("value")), !!sym("value") > 0)

    proximity_products[upper.tri(proximity_products, diag = T)] <- NA
    row_names <- rownames(proximity_products)

    proximity_products <- as.matrix(proximity_products) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = row_names) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!is.na(!!sym("value")), !!sym("value") > 0)
  }

  proximity <- list(proximity_countries = proximity_countries,
                    proximity_products = proximity_products)

  return(proximity)
}
