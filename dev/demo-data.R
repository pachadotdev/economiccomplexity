library(economiccomplexity2)

r <- rca(ec_trade_1962)
cf <- complexity(r, method = "fitness", atlas = T)
p <- proximity(r, cf$country_diversity, cf$product_ubiquity)

ec_output_demo <- list(
  rca = r,
  complexity = cf,
  proximity = p
)

save(ec_output_demo, file = "data/ec_output_demo.rda", compress = "xz")
