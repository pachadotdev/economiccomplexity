ba_ind <- balassa_index(world_trade_avg_1998_to_2000)

x <- test_eig(ba_ind, 10L)
y <- test_eig_(ba_ind, 10L)

dim(x)
dim(y)

head(x[1, ])
head(y[1, ])

com_fit <- complexity_measures(ba_ind)

prox <- proximity(ba_ind, "both")

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

save(economiccomplexity_output, file = "data/economiccomplexity_output.rda", compress = "xz")
