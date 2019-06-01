set.seed(1810)
library(economiccomplexity)

rca <- revealed_comparative_advantage(d = world_trade_1980, c = "reporter_iso", p = "product_code", x = "export_value_usd", output = "matrix")

indices <- economic_complexity_measures(d = rca, method = "reflections", iterations = 20, output = "tibble")

proximity <- proximity(indices$m, indices$kc0, indices$kp0, output = "matrix")

networks <- networks(proximity$proximity_countries, proximity$proximity_products,
                     c_cutoff = 0.35, p_cutoff = 0.55)

library(igraph)
coords <- layout.fruchterman.reingold(networks$products_network)
plot(networks$products_network, layout = coords)
