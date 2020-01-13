library(binet)

trade <- readRDS("~/github/binet/dev/world_trade_avg_1998_to_2000.rds")

bi <- balassa_index(
  data = trade,
  source = "reporter_iso",
  target = "product_code",
  value = "trade_value_usd"
)

com_fit <- complexity_measures(balassa_index = bi)

pro <- proximity(
  balassa_index = bi,
  balassa_sum_source = com_fit$balassa_sum_source,
  balassa_sum_target = com_fit$balassa_sum_target
)

net <- projections(
  proximity_source = pro$proximity_source,
  proximity_target = pro$proximity_target,
  tolerance = 0.05
)

aggregated_countries <- trade %>%
  group_by(reporter_iso) %>%
  summarise(trade_value_usd = sum(trade_value_usd))

aggregated_products <- trade %>%
  group_by(product_code) %>%
  summarise(trade_value_usd = sum(trade_value_usd))

set.seed(200100)

g_countries <- net$network_source %>%
  graph_from_data_frame(vertices = aggregated_countries, directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = trade_value_usd), color = "darkslategray4") +
  scale_size(range = c(2,10)) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Planets") +
  theme_void()

g_products <- net$network_target %>%
  graph_from_data_frame(vertices = aggregated_products, directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 4) +
  geom_node_point(aes(size = trade_value_usd), color = "darkslategray4") +
  scale_size(range = c(2,10)) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void()

data.table::fwrite(net$network_target, "dev/product_space_replication.csv")
