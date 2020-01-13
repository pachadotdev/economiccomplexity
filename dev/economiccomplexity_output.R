ba_ind <- balassa_index(
  data = galactic_federation, source = "planet", target = "product", value = "export_value"
)

com_fit <- complexity_measures(ba_ind)

prox <- proximity(
  balassa_index = ba_ind,
  balassa_sum_source = com_fit$balassa_sum_source,
  balassa_sum_target = com_fit$balassa_sum_target
)

# proj <- projections(
#   proximity_source = prox$proximity_source,
#   proximity_target = prox$proximity_target,
#   tolerance = 0.01,
#   avg_links = 4
# )

economiccomplexity_output <- list(
  balassa_index = ba_ind,
  complexity_measures = com_fit,
  proximity = prox
)

save(economiccomplexity_output, file = "data/economiccomplexity_output.rda")
