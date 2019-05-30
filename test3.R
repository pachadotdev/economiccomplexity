#Phi_pp <- (t(Mcp_4) %*% Mcp_4) / proximity_products_denominator(Mcp_4, U_4, cores = min(1,n_cores))

x = indices(m = rca(d = world_trade_1980, c = "reporter_iso",
                    p = "product_code", x = "export_value_usd"),
            method = "reflections", maxiter = 20, output = "matrix")

m <- x$m
kc0 <- x$kc0
kp0 <- x$kp0

phi_pp_up <- Matrix::t(m) %*% m

phi_pp_down <- dplyr::tibble(i = 1:length(kp0), kp0 = kp0)

phi_pp_down <- expand.grid(i = 1:length(kp0), j = 1:length(kp0))

phi_pp_down <- phi_pp_down %>%
  dplyr::as_tibble() %>%
  dplyr::left_join(phi_pp_down, by = "i") %>%
  dplyr::left_join(phi_pp_down, by = c("j" = "i")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(kp0 = max(kp0.x, kp0.y)) %>%
  dplyr::ungroup()

phi_pp_down <- Matrix::Matrix(kp0_ij$kp0, nrow = ncol(m), ncol = ncol(m), sparse = T)


for (i in 1:ncol(m)) {
  for (j in 1:ncol(m)) {
    if (j > i) {
      break
    } else {
      phi_pp_down[i,j] <- max(kp0[i], kp0[j])
    }
  }
}

phi_pp_down_i <- apply(phi_pp_down, 1:2, function(i,j) { pmax(kp0[i], kp0[j]) })
phi_pp_down_j <- apply(phi_pp_down, 1:2, function(j) kp0[j])
