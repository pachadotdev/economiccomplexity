## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE, tidy = FALSE----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library(economiccomplexity)

services_trade_2016$services_trade_2016_tibble

## ------------------------------------------------------------------------
rca <- revealed_comparative_advantage(
  trade_data = services_trade_2016$services_trade_2016_tibble, 
  country = "country",
  product = "product", 
  value = "value"
)

# 5x5 preview
rca[1:5,1:3]

## ------------------------------------------------------------------------
rca_tbl <- revealed_comparative_advantage(
  trade_data = services_trade_2016$services_trade_2016_tibble, 
  country = "country",
  product = "product", 
  value = "value",
  tbl_output = T
)

rca_tbl

## ------------------------------------------------------------------------
rca_decimal <- revealed_comparative_advantage(
  trade_data = services_trade_2016$services_trade_2016_tibble, 
  country = "country",
  product = "product", 
  value = "value",
  discrete = F
)

# 5x3 preview
rca_decimal[1:5,1:3]

## ------------------------------------------------------------------------
rca_decimal_tbl <- revealed_comparative_advantage(
  trade_data = services_trade_2016$services_trade_2016_tibble, 
  country = "country",
  product = "product", 
  value = "value",
  tbl_output = T,
  discrete = F
)

rca_decimal_tbl

## ------------------------------------------------------------------------
cm_reflections <- complexity_measures(
  rca, 
  method = "reflections", 
  tbl_output = T
)

cm_reflections$economic_complexity_index
cm_reflections$product_complexity_index

## ------------------------------------------------------------------------
cm_eigenvalues <- complexity_measures(
  rca,
  method = "eigenvalues",
  tbl_output = T
)

cm_eigenvalues$economic_complexity_index
cm_eigenvalues$product_complexity_index

## ------------------------------------------------------------------------
cm_fitness <- complexity_measures(
  rca,
  method = "fitness", 
  tbl_output = T
)

cm_fitness$economic_complexity_index
cm_fitness$product_complexity_index

## ------------------------------------------------------------------------
pro <- proximity(
  rca,
  diversity = cm_fitness$diversity,
  ubiquity = cm_fitness$ubiquity,
  tbl_output = T
)

pro$proximity_countries
pro$proximity_products

## ------------------------------------------------------------------------
net <- networks(
  pro$proximity_countries,
  pro$proximity_products,
  c_cutoff = 0.7,
  p_cutoff = 0.1,
  tbl_output = T
)

net$network_countries
net$network_products

## ---- fig.width=20, fig.height=12----------------------------------------
library(igraph)
library(ggraph)
library(magrittr)

set.seed(200100)

net$network_countries %>% 
  graph_from_data_frame(directed = F) %>% 
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value), edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle(
    expression(paste("Connections between",
                     italic(" countries "),
                     "based on the",
                     italic(" services "),
                     "they export"))) +
  theme_void()

## ---- fig.width=20, fig.height=12----------------------------------------
net$network_products %>% 
  graph_from_data_frame(directed = F) %>% 
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value), edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle(
    expression(paste("Connections between",
                     italic(" services "),
                     "based on the",
                     italic(" countries "),
                     "that export them"))) +
  theme_void()

