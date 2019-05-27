#' RCA (Revealed Comparative Advantage)
#'
#' @export
#' @param m "wide" matrix
#' @param maxiter 20
#' @param method "reflections" or "eigenvalues"
#' @param output "matrix" or "tibble"
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble select mutate
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym
#' @examples
#' indices(m = rca(d = world_trade_1980, c = "reporter_iso", p = "product_code", x = "export_value_usd"),
#' method = "reflections", maxiter = 20, output = "matrix")
#' @keywords functions

indices <- function(m, maxiter = 20, method = "reflections" , output = "matrix") {
  m2 <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]

  # diversity and ubiquity following the Atlas notation
  kc0 <- Matrix::rowSums(m2)
  kp0 <- Matrix::colSums(m2)

  kcinv <- 1 / kc0
  kpinv <- 1 / kp0

  if (method == "reflections") {
    # create empty matrices
    kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = maxiter, sparse = T)
    kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = maxiter, sparse = T)

    # fill the first column with kc0 and kp0 to start iterating
    kc[, 1] <- kc0
    kp[, 1] <- kp0

    # compute cols 2 to maxiter by iterating from col 1
    for (j in 2:ncol(kc)) {
      kc[, j] <- (m2 %*% kp[, (j - 1)]) * kcinv
      kp[, j] <- (Matrix::t(m2) %*% kc[, (j - 1)]) * kpinv
    }

    eci <- (kc[, maxiter - 1] - base::mean(kc[, maxiter - 1])) / stats::sd(kc[, maxiter - 1])
    names(eci) <- rownames(m2)

    pci <- (kp[, maxiter] - base::mean(kp[, maxiter])) / stats::sd(kp[, maxiter])
    names(pci) <- colnames(m2)
  }

  if (method == "eigenvalues") {
    eci <- eigen((m2 %*% Matrix::t(m2 * kpinv)) * kcinv)
    eci <- eci$vectors[,1]
    names(eci) <- rownames(m2)
    eci <- (eci - base::mean(eci)) / stats::sd(eci)

    pci <- eigen((Matrix::t(m2) %*% (m2 * kcinv)) * kpinv)
    pci <- pci$vectors[,1]
    names(pci) <- colnames(m2)
    pci <- (pci - base::mean(pci)) /  stats::sd(pci)
  }

  if (output == "tibble") {
    eci <- dplyr::tibble(v = eci) %>%
      dplyr::mutate(c = names(eci)) %>%
      dplyr::select(!!sym("c"), !!sym("v"))

    pci <- dplyr::tibble(v = pci) %>%
      dplyr::mutate(p = names(pci)) %>%
      dplyr::select(!!sym("p"), !!sym("v"))
  }

  indices <- list(eci = eci, pci = pci)

  return(indices)
}
