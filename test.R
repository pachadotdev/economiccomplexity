library(economiccomplexity)

rca <- rca(d = world_trade_1980, c = "reporter_iso",
    p = "product_code", x = "export_value_usd")

indices <- indices(rca, method = "reflections", maxiter = 20,
    output = "matrix")

m <- indices$m
kc0 <- indices$kc0
kp0 <- indices$kp0

kc <- Matrix::drop(kc0)
kc <- outer(kc, kc, pmax)

kp <- Matrix::drop(kp0)
kp <- outer(kp0, kp0, pmax)

proximity <- proximity(m, kc = kc0, kp = kp0)
