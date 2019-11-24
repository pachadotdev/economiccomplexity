library(economiccomplexity)

r <- rca(ec_trade_1962)
cf <- complexity(r, method = "fitness", atlas = T)
p <- proximity(r, cf$diversity, cf$ubiquity)

ec_output_demo <- list(
  rca = r,
  complexity = cf,
  proximity = p
)

save(ec_output_demo, file = "data/ec_output_demo.rda", compress = "xz")
