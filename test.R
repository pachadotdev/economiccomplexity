# world_trade_1980 <- tradestatistics::ots_create_tidy_data(years = 1980, reporters = "all", partners = "all", table = "yrc")
# world_trade_1980 <- dplyr::select(world_trade_1980, reporter_iso, product_code, export_value_usd)

m <- economiccomplexity::rca(d = world_trade_1980, c = "reporter_iso", p = "product_code", x = "export_value_usd", discrete = T, output = "matrix")

indices <- function(m, maxiter = 20, output = "tibble") {
  m2 <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]

  # diversity and ubiquity following the Atlas notation
  kc0 <- Matrix::rowSums(m2)
  kp0 <- Matrix::colSums(m2)

  # reflections method ------------------------------------------------------

  kcinv <- 1 / kc0
  kpinv <- 1 / kp0

  # create empty matrices
  kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = maxiter, sparse = T)
  kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = maxiter, sparse = T)

  # fill the first column with kc0 and kp0 to start iterating
  kc[, 1] <- kc0
  kp[, 1] <- kp0

  # compute cols 2 to maxiter by iterating from col 1
  for (j in 2:ncol(kc)) {
    kc[, j] <- kcinv * (m2 %*% kp[, (j - 1)])
    kp[, j] <- kpinv * (Matrix::t(m2) %*% kc[, (j - 1)])
  }

  eci_reflections <- (kc[, maxiter - 1] - mean(kc[, maxiter - 1])) / sd(kc[, maxiter - 1])
  names(eci_reflections) <- rownames(m2)

  pci_reflections <- (kp[, maxiter] - mean(kp[, maxiter])) / sd(kp[, maxiter])
  names(pci_reflections) <- colnames(m2)

  if (output == "tibble") {
    eci_reflections <- dplyr::tibble(v = eci_reflections) %>%
      mutate(c = names(eci_reflections)) %>%
      select(!!sym("c"), v)

    pci_reflections <- dplyr::tibble(v = pci_reflections) %>%
      mutate(p = names(pci_reflections)) %>%
      select(!!sym("p"), v)
  }

  indices <- list(eci_reflections = eci_reflections, pci_reflections = pci_reflections)
  return(indices)
}

a = indices(m)

# eci = (m2 %*% Matrix::t(m2 * kpinv)) * kcinv
# dim(eci)
# eci = eigen(eci)
# eci = as.vector(eci$vectors[,1])
# names(eci) <- rownames(m2)
# eci = (eci - mean(eci)) / sd(eci)
# sort(eci, decreasing = T)

i1 = indices(m = rca(d = world_trade_1980, c = "reporter_iso", p = "product_code", x = "export_value_usd"),
        method = "reflections", maxiter = 20, output = "tibble")

i2 = indices(m = rca(d = world_trade_1980, c = "reporter_iso", p = "product_code", x = "export_value_usd"),
             method = "eigenvalues", maxiter = 20, output = "tibble")

i1$eci %>% arrange(-v)
i2$eci %>% arrange(-v)
