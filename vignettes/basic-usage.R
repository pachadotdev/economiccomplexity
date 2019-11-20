## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library(economiccomplexity)

ec_trade_1962

## ------------------------------------------------------------------------
r <- rca(ec_trade_1962)
r

## ------------------------------------------------------------------------
r2 <- rca(ec_trade_1962, tbl = F)
r2[1:5,1:5] # 5x5 preview

## ------------------------------------------------------------------------
r3 <- rca(ec_trade_1962, discrete = F)
r3

## ------------------------------------------------------------------------
r4 <- rca(ec_trade_1962, discrete = F, tbl = F)
r4[1:5,1:5] # 5x3 preview

## ------------------------------------------------------------------------
cf <- complexity(r, method = "fitness")
cf

## ------------------------------------------------------------------------
cr <- complexity(r, method = "reflections")
cr

## ------------------------------------------------------------------------
ce <- complexity(r, method = "eigenvalues")
ce

## ------------------------------------------------------------------------
p <- proximity(r, cf$country_diversity, cf$product_ubiquity)
p

## ------------------------------------------------------------------------
# net <- ec_networks(
#   pc = pro$proximity_c,
#   pp = pro$proximity_p,
#   cutoff_c = 1,
#   cutoff_p = 1,
#   tbl = T
# )
# 
# net$network_c
# net$network_p

## ---- fig.width=20, fig.height=12----------------------------------------
# library(igraph)
# library(ggraph)
# library(magrittr)
# 
# set.seed(200100)
# 
# net$network_c %>%
#   graph_from_data_frame(directed = F) %>%
#   ggraph(layout = "kk") +
#   geom_edge_link(aes(edge_alpha = value, edge_width = value),
#                  edge_colour = "#a8a8a8") +
#   geom_node_point(color = "darkslategray4", size = 8) +
#   geom_node_text(aes(label = name), vjust = 2.2) +
#   ggtitle("The Country Space") +
#   theme_void()
# ```
# 
# ```{r, fig.width=20, fig.height=12}
# net$network_p %>%
#   graph_from_data_frame(directed = F) %>%
#   ggraph(layout = "kk") +
#   geom_edge_link(aes(edge_alpha = value, edge_width = value),
#                  edge_colour = "#a8a8a8") +
#   geom_node_point(color = "darkslategray4", size = 4) +
#   geom_node_text(aes(label = name), vjust = 2.2) +
#   ggtitle("The Product Space") +
#   theme_void()

