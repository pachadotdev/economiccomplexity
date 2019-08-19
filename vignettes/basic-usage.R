## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library(economiccomplexity)

ec_trade_1962

## ------------------------------------------------------------------------
rca <- ec_rca(
  data = ec_trade_1962,
  c = "country",
  p = "product",
  v = "value"
)

# 5x5 preview
rca[1:5,1:3]

## ------------------------------------------------------------------------
rca_tbl <- ec_rca(
  data = ec_trade_1962,
  c = "country",
  p = "product",
  v = "value",
  tbl = T
)

rca_tbl

## ------------------------------------------------------------------------
rca_decimal <- ec_rca(
  data = ec_trade_1962,
  c = "country",
  p = "product",
  v = "value",
  discrete = F
)

# 5x3 preview
rca_decimal[1:5,1:3]

## ------------------------------------------------------------------------
rca_decimal_tbl <- ec_rca(
  data = ec_trade_1962,
  c = "country",
  p = "product",
  v = "value",
  tbl = T,
  discrete = F
)

rca_decimal_tbl

## ------------------------------------------------------------------------
cm_reflections <- ec_complexity_measures(
  rca = rca,
  method = "reflections",
  tbl = T
)

cm_reflections$complexity_index_c
cm_reflections$complexity_index_p

## ------------------------------------------------------------------------
cm_eigenvalues <- ec_complexity_measures(
  rca = rca,
  method = "eigenvalues",
  tbl = T
)

cm_eigenvalues$complexity_index_c
cm_eigenvalues$complexity_index_p

## ------------------------------------------------------------------------
cm_fitness <- ec_complexity_measures(
  rca = rca,
  method = "fitness",
  tbl = T
)

cm_fitness$complexity_index_c
cm_fitness$complexity_index_p

## ------------------------------------------------------------------------
pro <- ec_proximity(
  rca = rca,
  d = cm_fitness$diversity,
  u = cm_fitness$ubiquity,
  tbl = T
)

pro$proximity_c
pro$proximity_p

## ------------------------------------------------------------------------
net <- ec_networks(
  pc = pro$proximity_c,
  pp = pro$proximity_p,
  cutoff_c = 1,
  cutoff_p = 1,
  tbl = T
)

net$network_c
net$network_p

## ---- fig.width=20, fig.height=12----------------------------------------
library(igraph)
library(ggraph)
library(magrittr)

set.seed(200100)

net$network_c %>%
  graph_from_data_frame(directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("The Country Space") +
  theme_void()

## ---- fig.width=20, fig.height=12----------------------------------------
net$network_p %>%
  graph_from_data_frame(directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 4) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("The Product Space") +
  theme_void()

