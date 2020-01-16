## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE-----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(economiccomplexity)

galactic_federation

## -----------------------------------------------------------------------------
bi <- balassa_index(galactic_federation)
bi

## -----------------------------------------------------------------------------
bi_dec <- balassa_index(galactic_federation, discrete = F)
bi_dec

## -----------------------------------------------------------------------------
com_fit <- complexity_measures(bi)
com_fit$complexity_index_country
com_fit$complexity_index_product

## -----------------------------------------------------------------------------
com_ref <- complexity_measures(bi, method = "reflections")
com_ref$complexity_index_country
com_ref$complexity_index_product

## -----------------------------------------------------------------------------
com_eig <- complexity_measures(bi, method = "eigenvalues")
com_eig$complexity_index_country
com_eig$complexity_index_product

## -----------------------------------------------------------------------------
pro <- proximity(bi)
pro$proximity_country
pro$proximity_product

## -----------------------------------------------------------------------------
net <- projections(pro$proximity_country, pro$proximity_product)
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

