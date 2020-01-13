## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE-----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(binet)

galactic_federation

## -----------------------------------------------------------------------------
bi <- balassa_index(
  data = galactic_federation,
  source = "planet",
  target = "product",
  value = "export_value"
)

bi

## -----------------------------------------------------------------------------
bi_dec <- balassa_index(
  data =galactic_federation,
  source = "planet",
  target = "product",
  value = "export_value",
  discrete = F
)

bi_dec

## -----------------------------------------------------------------------------
com_fit <- complexity_measures(balassa_index = bi)

com_fit$complexity_index_source
com_fit$complexity_index_target

## -----------------------------------------------------------------------------
com_ref <- complexity_measures(
  balassa_index = bi,
  method = "reflections"
)

com_ref$complexity_index_source
com_ref$complexity_index_target

## -----------------------------------------------------------------------------
com_eig <- complexity_measures(
  balassa_index = bi,
  method = "eigenvalues"
)

com_eig$complexity_index_source
com_eig$complexity_index_target

## -----------------------------------------------------------------------------
pro <- proximity(
  balassa_index = bi,
  balassa_sum_source = com_fit$balassa_sum_source,
  balassa_sum_target = com_fit$balassa_sum_target
)

pro$proximity_source
pro$proximity_target

## -----------------------------------------------------------------------------
net <- projections(
  proximity_source = pro$proximity_source,
  proximity_target = pro$proximity_target,
  avg_links = 4,
  tolerance = 0.05
)

net$network_source
net$network_target

## ---- fig.width=7, fig.height=7-----------------------------------------------
set.seed(200100)
library(Matrix)
library(igraph)
library(ggraph)

aggregated_planets <- rowSums(galactic_federation)
V(net$network_source)$size <- aggregated_planets[match(V(net$network_source)$name, names(aggregated_planets))]

net$network_source %>% 
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "darkslategray4") +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Planets") +
  theme_void()

## ---- fig.width=7, fig.height=7-----------------------------------------------
set.seed(200100)

aggregated_products <- colSums(galactic_federation)
V(net$network_target)$size <- aggregated_products[match(V(net$network_target)$name, names(aggregated_products))]

net$network_target %>% 
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "darkslategray4") +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void()

