## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE, tidy = FALSE----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library(economiccomplexity)

world_trade_2017

## ------------------------------------------------------------------------
rca <- revealed_comparative_advantage(
  d = world_trade_2017,
  c = "reporter_iso",
  p = "product_code",
  v = "export_value_usd"
)

rca[1:5, 1:5]

## ------------------------------------------------------------------------
rca_tbl <- revealed_comparative_advantage(
  d = world_trade_2017,
  c = "reporter_iso",
  p = "product_code",
  v = "export_value_usd",
  tbl_output = T
)

rca_tbl

## ------------------------------------------------------------------------
rca_decimal <- revealed_comparative_advantage(
  d = world_trade_2017,
  c = "reporter_iso",
  p = "product_code",
  v = "export_value_usd",
  tbl_output = F,
  discrete = F
)

rca_decimal[1:5, 1:5]

## ------------------------------------------------------------------------
rca_decimal_tbl <- revealed_comparative_advantage(
  d = world_trade_2017,
  c = "reporter_iso",
  p = "product_code",
  v = "export_value_usd",
  tbl_output = T,
  discrete = T
)

rca_decimal_tbl

## ------------------------------------------------------------------------
complexity_measures_reflections <- complexity_measures(
  rca,
  method = "reflections",
  tbl_output = T
)

complexity_measures_reflections$economic_complexity_index
complexity_measures_reflections$product_complexity_index

## ------------------------------------------------------------------------
complexity_measures_eigenvalues <- complexity_measures(
  rca,
  method = "eigenvalues",
  tbl_output = T
)

complexity_measures_eigenvalues$economic_complexity_index
complexity_measures_eigenvalues$product_complexity_index

## ------------------------------------------------------------------------
complexity_measures_fitness <- complexity_measures(
  rca,
  method = "fitness",
  tbl_output = T
)

complexity_measures_fitness$economic_complexity_index
complexity_measures_fitness$product_complexity_index

## ------------------------------------------------------------------------
proximity <- proximity_matrices(
  rca,
  diversity = complexity_measures_fitness$diversity,
  ubiquity = complexity_measures_fitness$ubiquity
)

proximity$countries_proximity[1:5, 1:5]
proximity$products_proximity[1:5, 1:5]

## ------------------------------------------------------------------------
networks <- proximity_networks(
  proximity$countries_proximity,
  proximity$products_proximity,
  c_cutoff = 0.3,
  p_cutoff = 0.6,
  tbl_output = T
)

networks$countries_network
networks$products_network

## ------------------------------------------------------------------------
library(igraph)

set.seed(1810)

countries_network <- graph_from_data_frame(networks$countries_network, directed = F)

plot(
  countries_network,
  layout = layout_with_kk,
  vertex.size = 5,
  vertex.color = "#a8a8a8",
  vertex.label.dist = 1,
  vertex.label.color = "black"
)
