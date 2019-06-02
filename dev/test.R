set.seed(1810)
library(economiccomplexity)

rca <- revealed_comparative_advantage(d = world_trade_2017, c = "reporter_iso",
                                      p = "product_code", v = "export_value_usd", tbl_output = T)

measures <- economic_complexity_measures(d = rca, use_eigenvalues = F, tbl_output = F)

proximity <- proximity_matrices(d = rca, diversity = measures$diversity, ubiquity = measures$ubiquity, tbl_output = T)

networks <- networks(proximity$proximity_countries, proximity$proximity_products,
                     c_cutoff = 0.55, p_cutoff = 0.35, tbl_output = T)

library(igraph)
coords <- layout.fruchterman.reingold(networks$products_network)
plot(networks$products_network, layout = coords)
