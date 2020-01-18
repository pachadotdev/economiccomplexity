library(economiccomplexity)
library(dplyr)
library(igraph)
library(ggraph)

trade <- readRDS("~/github/economiccomplexity/dev/world_trade_avg_1998_to_2000.rds")

bi <- balassa_index(
  data = trade,
  country = "reporter_iso",
  product = "product_code",
  value = "trade_value_usd"
)

com_fit <- complexity_measures(balassa_index = bi)

pro <- proximity(
  balassa_index = bi
)

net <- projections(
  proximity_country = pro$proximity_country,
  proximity_product = pro$proximity_product,
  tolerance = 0.05
)

aggregated_products <- trade %>%
  group_by(product_code) %>%
  summarise(trade_value_usd = sum(trade_value_usd))

aggregated_products <- setNames(aggregated_products$trade_value_usd, aggregated_products$product_code)
V(net$network_product)$size <- aggregated_products[match(V(net$network_product)$name, names(aggregated_products))]

set.seed(200100)

g_products <- net$network_product %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_edge_link(edge_colour = "#888888") +
  geom_node_point(aes(size = size), color = "#86494d") +
  # geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void()

g_products
