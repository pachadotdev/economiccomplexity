## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE-----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(economiccomplexity)

galactic_federation

## -----------------------------------------------------------------------------
bi <- balassa_index(
  data = galactic_federation,
  country = "planet",
  product = "product",
  value = "export_value"
)

bi

## -----------------------------------------------------------------------------
bi_dec <- balassa_index(
  data =galactic_federation,
  country = "planet",
  product = "product",
  value = "export_value",
  discrete = F
)

bi_dec

## -----------------------------------------------------------------------------
com_fit <- complexity_measures(balassa_index = bi)

com_fit$complexity_index_country
com_fit$complexity_index_product

## -----------------------------------------------------------------------------
com_ref <- complexity_measures(
  balassa_index = bi,
  method = "reflections"
)

com_ref$complexity_index_country
com_ref$complexity_index_product

## -----------------------------------------------------------------------------
com_eig <- complexity_measures(
  balassa_index = bi,
  method = "eigenvalues"
)

com_eig$complexity_index_country
com_eig$complexity_index_product

## -----------------------------------------------------------------------------
pro <- proximity(
  balassa_index = bi,
  balassa_sum_country = com_fit$balassa_sum_country,
  balassa_sum_product = com_fit$balassa_sum_product
)

pro$proximity_country
pro$proximity_product

## -----------------------------------------------------------------------------
net <- projections(
  proximity_country = pro$proximity_country,
  proximity_product = pro$proximity_product,
  avg_links = 4,
  tolerance = 0.05
)

net$network_country
net$network_product

## ---- fig.width=7, fig.height=7-----------------------------------------------
set.seed(200100)
library(Matrix)
library(igraph)
library(ggraph)

aggregated_planets <- rowSums(galactic_federation)
V(net$network_country)$size <- aggregated_planets[match(V(net$network_country)$name, names(aggregated_planets))]

net$network_country %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "darkslategray4") +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Planets") +
  theme_void()

## ---- fig.width=7, fig.height=7-----------------------------------------------
set.seed(200100)

aggregated_products <- colSums(galactic_federation)
V(net$network_product)$size <- aggregated_products[match(V(net$network_product)$name, names(aggregated_products))]

net$network_product %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "darkslategray4") +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void()

