library(economiccomplexity)

rca <- rca(d = world_trade_1980, c = "reporter_iso",
    p = "product_code", x = "export_value_usd")

indices <- indices(rca, method = "reflections", maxiter = 20,
    output = "matrix")

m <- indices$m
kc0 <- indices$kc0
kp0 <- indices$kp0

proximity <- proximity(m, kc = kc0, kp = kp0)
