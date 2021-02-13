## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE-----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(economiccomplexity)

# partial view of trade matrix
head(world_trade_avg_1998_to_2000)

# partial view of gdp vector
head(world_gdp_avg_1998_to_2000)

## -----------------------------------------------------------------------------
bi <- balassa_index(world_trade_avg_1998_to_2000)

# partial view of index
bi[1:5,1:5]

## -----------------------------------------------------------------------------
bi_dec <- balassa_index(world_trade_avg_1998_to_2000, discrete = F)

# partial view of index
bi_dec[1:5,1:5]

## -----------------------------------------------------------------------------
com_fit <- complexity_measures(bi)

# partial view of indexes
com_fit$complexity_index_country[1:5]
com_fit$complexity_index_product[1:5]

## -----------------------------------------------------------------------------
com_ref <- complexity_measures(bi, method = "reflections")

# partial view of indexes
com_ref$complexity_index_country[1:5]
com_ref$complexity_index_product[1:5]

## -----------------------------------------------------------------------------
com_eig <- complexity_measures(bi, method = "eigenvalues")

# partial view of indexes
com_eig$complexity_index_country[1:5]
com_eig$complexity_index_product[1:5]

## -----------------------------------------------------------------------------
pro <- proximity(bi)

# partial view of proximity matrices
pro$proximity_country[1:5,1:5]
pro$proximity_product[1:5,1:5]

## -----------------------------------------------------------------------------
library(igraph)

net <- projections(pro$proximity_country, pro$proximity_product)

# partial view of projections
E(net$network_country)[1:5]
E(net$network_product)[1:5]

## ---- fig.width=7, fig.height=7-----------------------------------------------
set.seed(200100)

library(Matrix)
library(ggraph)

aggregated_countries <- aggregate(
  world_trade_avg_1998_to_2000$value,
  by = list(country = world_trade_avg_1998_to_2000$country),
  FUN = sum
)

aggregated_countries <- setNames(aggregated_countries$x, aggregated_countries$country)

V(net$network_country)$size <- aggregated_countries[match(V(net$network_country)$name, names(aggregated_countries))]

ggraph(net$network_country, layout = "kk") +
  # geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "#86494d") +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Countries") +
  theme_void()

## ---- fig.width=10, fig.height=10---------------------------------------------
set.seed(200100)

aggregated_products <- aggregate(
  world_trade_avg_1998_to_2000$value,
  by = list(country = world_trade_avg_1998_to_2000$product),
  FUN = sum
)

aggregated_products <- setNames(aggregated_products$x, aggregated_products$country)

V(net$network_product)$size <- aggregated_products[match(V(net$network_product)$name, names(aggregated_products))]

ggraph(net$network_product, layout = "kk") +
  # geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "#86494d") +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void()

## -----------------------------------------------------------------------------
co <- complexity_outlook(
  economiccomplexity_output$balassa_index,
  economiccomplexity_output$proximity$proximity_product,
  economiccomplexity_output$complexity_measures$complexity_index_product
)

# partial view of complexity outlook
co$complexity_outlook_index[1:5]
co$complexity_outlook_gain[1:5,1:5]

## -----------------------------------------------------------------------------
pl <- productivity_levels(world_trade_avg_1998_to_2000, world_gdp_avg_1998_to_2000)

# partial view of productivity levels
pl$productivity_level_country[1:5]
pl$productivity_level_product[1:5]

