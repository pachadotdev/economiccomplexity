ba_ind <- balassa_index(
  data = galactic_federation, country = "planet", product = "product", value = "export_value"
)

com_fit <- complexity_measures(ba_ind)

prox <- proximity(
  balassa_index = ba_ind
)

# proj <- projections(
#   proximity_country = prox$proximity_country,
#   proximity_product = prox$proximity_product,
#   tolerance = 0.01,
#   avg_links = 4
# )

economiccomplexity_output <- list(
  balassa_index = ba_ind,
  complexity_measures = com_fit,
  proximity = prox
)

save(economiccomplexity_output, file = "data/economiccomplexity_output.rda")
