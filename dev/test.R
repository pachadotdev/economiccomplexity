library(economiccomplexity)

rca <- revealed_comparative_advantage(
  d = world_trade_2017, c = "reporter_iso",
  p = "product_code", v = "export_value_usd"
)

complexity_measures <- economic_complexity_measures(world_rca_2017, method = "fitness", tbl_output = T)

proximity <- proximity_matrices(
  d = world_rca_2017,
  diversity = complexity_measures_2017$diversity,
  ubiquity = complexity_measures_2017$ubiquity
)

networks <- networks(proximity_matrices_2017$countries_proximity,
  proximity_matrices_2017$products_proximity,
  tbl_output = T
)

data.table::fwrite(networks$products_network, "dev/products_network.csv")
