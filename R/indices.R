#' Indices
#'
#' @export
#' @param d "wide" matrix
#' @param method "reflections" or "eigenvalues"
#' @param iterations only used for the reflections method (default set to 20)
#' @param output "matrix" or "tibble"
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble tibble select mutate
#' @importFrom tidyr gather
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym
#' @examples
#' rca <- rca(d = world_trade_1980, c = "reporter_iso",
#'     p = "product_code", x = "export_value_usd")
#'
#' indices <- indices(rca, method = "reflections", iterations = 20,
#'     output = "matrix")
#' @keywords functions

economic_complexity_measures <- function(d, method = "reflections", iterations = 20, output = "matrix") {
  if (!method %in% c("reflections", "eigenvalues")) {
    stop()
  }

  if (!iterations >= 2 & is.integer(iterations)) {
    stop()
  }

  if (!output %in% c("matrix", "tibble")) {
    stop()
  }

  if (is.data.frame(d)) {
    m <- spread(d, !!sym("product"), !!sym("value"))
    c <- m$country

    m <- as.matrix(dplyr::select(m, -!!sym("country")))
    m[is.na(m)] <- 0
    rownames(m) <- c

    m <- Matrix::Matrix(m, sparse = T)
  } else {
    m <- d[Matrix::rowSums(d) != 0, Matrix::colSums(d) != 0]
  }

  # diversity and ubiquity following the Atlas notation
  kc0 <- Matrix::rowSums(m)
  kp0 <- Matrix::colSums(m)

  if (method == "reflections") {
    # create empty matrices
    kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = iterations, sparse = T)
    kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = iterations, sparse = T)

    # fill the first column with kc0 and kp0 to start iterating
    kc[, 1] <- kc0
    kp[, 1] <- kp0

    # compute cols 2 to iterations by iterating from col 1
    for (j in 2:ncol(kc)) {
      kc[, j] <- (m %*% kp[, (j - 1)]) * (1 / kc0)
      kp[, j] <- (Matrix::t(m) %*% kc[, (j - 1)]) * (1 / kp0)
    }

    eci <- (kc[, iterations - 1] - base::mean(kc[, iterations - 1])) /
      stats::sd(kc[, iterations - 1])

    pci <- (kp[, iterations] - base::mean(kp[, iterations])) /
      stats::sd(kp[, iterations])
  }

  if (method == "eigenvalues") {
    eci <- eigen((m %*% Matrix::t(m * (1 / kp0))) * (1 / kc0))
    eci <- eci$vectors[,1]
    eci <- (eci - base::mean(eci)) / stats::sd(eci)

    pci <- eigen((Matrix::t(m) %*% (m * (1 / kc0))) * (1 / kp0))
    pci <- pci$vectors[,1]
    pci <- (pci - base::mean(pci)) /  stats::sd(pci)
  }

  names(eci) <- rownames(m)
  names(pci) <- colnames(m)

  if (output == "tibble") {
    eci <- dplyr::tibble(value = eci) %>%
      dplyr::mutate(country = names(eci)) %>%
      dplyr::select(!!sym("country"), !!sym("value"))

    pci <- dplyr::tibble(value = pci) %>%
      dplyr::mutate(product = names(pci)) %>%
      dplyr::select(!!sym("product"), !!sym("value"))

    m <- as.matrix(m) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(country = rownames(m)) %>%
      tidyr::gather(!!sym("product"), !!sym("value"), -!!sym("country"))
  }

  economic_complexity_measures <- list(
    economic_complexity_index = eci,
    product_complexity_index = pci,
    diversity = kc0,
    ubiquity = kp0,
    revealed_comparative_advantage = m
  )

  return(economic_complexity_measures)
}
