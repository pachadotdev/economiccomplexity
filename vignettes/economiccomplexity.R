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
bi[1:5, 1:5]

## -----------------------------------------------------------------------------
bi_dec <- balassa_index(world_trade_avg_1998_to_2000, discrete = F)

# partial view of index
bi_dec[1:5, 1:5]

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
pro$proximity_country[1:5, 1:5]
pro$proximity_product[1:5, 1:5]

## -----------------------------------------------------------------------------
library(igraph)

net <- projections(pro$proximity_country, pro$proximity_product)

# partial view of projections
E(net$network_country)[1:5]
E(net$network_product)[1:5]

## -----------------------------------------------------------------------------
ecount(net$network_country)
ecount(net$network_product)

edge_density(net$network_country, loops = F)
edge_density(net$network_product, loops = F)

diameter(net$network_country, directed = F, unconnected = F)
diameter(net$network_product, directed = F, unconnected = F)

transitivity(net$network_country, type = "global")
transitivity(net$network_product, type = "global")

## -----------------------------------------------------------------------------
deg_country <- degree(net$network_country)
deg_product <- degree(net$network_product)

# country with the highest degree centrality
deg_country[which.max(deg_country)]

# product with the highest degree centrality
deg_product[which.max(deg_product)]

## -----------------------------------------------------------------------------
bet_country <- betweenness(net$network_country)
bet_product <- betweenness(net$network_product)

clo_country <- closeness(net$network_country)
clo_product <- closeness(net$network_product)

eig_country <- eigen_centrality(net$network_country)$vector
eig_product <- eigen_centrality(net$network_product)$vector

# country with the highest betweenness centrality
bet_country[which.max(bet_country)]

# product with the highest betweenness centrality
bet_product[which.max(bet_product)]

# country with the highest closeness centrality
clo_country[which.max(clo_country)]

# product with the highest closeness centrality
clo_product[which.max(clo_product)]

# country with the highest eigenvector centrality
eig_country[which.max(eig_country)]

# product with the highest eigenvector centrality
eig_product[which.max(eig_product)]

## -----------------------------------------------------------------------------
# sub-networks of the largest connected component

lcc_countries <- induced_subgraph(
  net$network_country,
  which(components(net$network_country)$membership ==
    which.max(components(net$network_country)$csize))
)

lcc_products <- induced_subgraph(
  net$network_product,
  which(components(net$network_product)$membership ==
    which.max(components(net$network_product)$csize))
)

# is this the same as the original networks?

ecount(lcc_countries) == ecount(net$network_country)
vcount(lcc_countries) == vcount(net$network_product)

deg2_countries <- degree(lcc_countries)
deg2_products <- degree(lcc_products)

bet2_countries <- betweenness(lcc_countries)
bet2_products <- betweenness(lcc_products)

clo2_countries <- closeness(lcc_countries)
clo2_products <- closeness(lcc_products)

eig2_countries <- eigen_centrality(lcc_countries)$vector
eig2_products <- eigen_centrality(lcc_products)$vector

all.equal(deg_country[which.max(deg_country)], deg2_countries[which.max(deg2_countries)])
all.equal(deg_product[which.max(deg_product)], deg2_products[which.max(deg2_products)])

all.equal(bet_country[which.max(bet_country)], bet2_countries[which.max(bet2_countries)])
all.equal(bet_product[which.max(bet_product)], bet2_products[which.max(bet2_products)])

all.equal(clo_country[which.max(clo_country)], clo2_countries[which.max(clo2_countries)])
all.equal(clo_product[which.max(clo_product)], clo2_products[which.max(clo2_products)])

all.equal(eig_country[which.max(eig_country)], eig2_countries[which.max(eig2_countries)])
all.equal(eig_product[which.max(eig_product)], eig2_products[which.max(eig2_products)])

## -----------------------------------------------------------------------------
k <- 4

# identify the core of the network
core_country <- coreness(net$network_country, mode = "all")
core_product <- coreness(net$network_product, mode = "all")

# identify the nodes in the core
kcore_country <- induced_subgraph(
  net$network_country,
  which(core_country >= k)
)

kcore_product <- induced_subgraph(
  net$network_product,
  which(core_product >= k)
)

V(kcore_country)$name

V(kcore_product)$name

## -----------------------------------------------------------------------------
# identify the backbone of the network
bbn_country <- delete_vertices(net$network_country, which(core_country < k))
bbn_product <- delete_vertices(net$network_product, which(core_product < k))

bbn_country
bbn_product

## -----------------------------------------------------------------------------
com_country <- cluster_fast_greedy(net$network_country)
com_product <- cluster_fast_greedy(net$network_product)

all.equal(vcount(net$network_country), length(com_country$membership))
all.equal(vcount(net$network_product), length(com_product$membership))

# number of communities
length(unique(com_country$membership))
length(unique(com_product$membership))

## -----------------------------------------------------------------------------
co <- complexity_outlook(
  economiccomplexity_output$balassa_index,
  economiccomplexity_output$proximity$proximity_product,
  economiccomplexity_output$complexity_measures$complexity_index_product
)

# partial view of complexity outlook
co$complexity_outlook_index[1:5]
co$complexity_outlook_gain[1:5, 1:5]

## -----------------------------------------------------------------------------
pl <- productivity_levels(world_trade_avg_1998_to_2000, world_gdp_avg_1998_to_2000)

# partial view of productivity levels
pl$productivity_level_country[1:5]
pl$productivity_level_product[1:5]

## -----------------------------------------------------------------------------
library(ggplot2)

deg_country <- data.frame(
  country = names(deg_country),
  deg = deg_country
)

deg_product <- data.frame(
  product = names(deg_product),
  deg = deg_product
)

ggplot(deg_country) +
  geom_histogram(aes(x = deg), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Degree Centrality Distribution for Countries")

ggplot(deg_product) +
  geom_histogram(aes(x = deg), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Degree Centrality Distribution for Products")

## -----------------------------------------------------------------------------
bet_country <- data.frame(
  country = names(bet_country),
  bet = bet_country
)

bet_product <- data.frame(
  product = names(bet_product),
  bet = bet_product
)

clo_country <- data.frame(
  country = names(clo_country),
  clo = clo_country
)

clo_product <- data.frame(
  product = names(clo_product),
  clo = clo_product
)

eig_country <- data.frame(
  country = names(eig_country),
  eig = eig_country
)

eig_product <- data.frame(
  product = names(eig_product),
  eig = eig_product
)

ggplot(bet_country) +
  geom_histogram(aes(x = bet), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Betweenness Centrality Distribution for Countries")

ggplot(bet_product) +
  geom_histogram(aes(x = bet), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Betweenness Centrality Distribution for Products")

ggplot(clo_country) +
  geom_histogram(aes(x = clo), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Closeness Centrality Distribution for Countries")

ggplot(clo_product) +
  geom_histogram(aes(x = clo), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Closeness Centrality Distribution for Products")

ggplot(eig_country) +
  geom_histogram(aes(x = eig), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Eigenvector Centrality Distribution for Countries")

ggplot(eig_product) +
  geom_histogram(aes(x = eig), bins = 20, fill = "#002948") +
  theme_minimal(base_size = 13) +
  labs(title = "Eigenvector Centrality Distribution for Products")

## ---- fig.width=7, fig.height=7-----------------------------------------------
set.seed(200100)

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
  geom_node_point(aes(size = size), color = "#002948") +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Countries") +
  theme_void()

## ---- fig.width=7, fig.height=7-----------------------------------------------
# Paint svn, ken and cze in yellow and the rest of the world in blue

V(net$network_country)$color <- rep(
  "Rest of the World",
  length(V(net$network_country)$size)
)
V(net$network_country)$color[match(
  c("svn", "ken", "cze"),
  V(net$network_country)$name
)] <- "Slovakia, Kenia and Czech Republic"

ggraph(net$network_country, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size, color = color)) +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Countries") +
  theme_void() +
  scale_colour_manual(values = c(
    "Slovakia, Kenia and Czech Republic" = "#fac704",
    "Rest of the World" = "#002948"
  ))

## ---- fig.width=10, fig.height=10---------------------------------------------
set.seed(200100)

aggregated_products <- aggregate(
  world_trade_avg_1998_to_2000$value,
  by = list(country = world_trade_avg_1998_to_2000$product),
  FUN = sum
)

aggregated_products <- setNames(aggregated_products$x, aggregated_products$country)

V(net$network_product)$size <- aggregated_products[
  match(V(net$network_product)$name, names(aggregated_products))
]

ggraph(net$network_product, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "#002948") +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void()

## ---- fig.width=10, fig.height=10---------------------------------------------
# Paint 8421, 7412 and 8434 in yellow and the rest of the products in blue

V(net$network_product)$color <- rep(
  "Rest of the Products",
  length(V(net$network_product)$size)
)
V(net$network_product)$color[match(
  c("8421", "7412", "8434"),
  V(net$network_product)$name
)] <- "8421, 7412 and 8434"

ggraph(net$network_product, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size, color = color)) +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void() +
  scale_colour_manual(values = c(
    "8421, 7412 and 8434" = "#fac704",
    "Rest of the Products" = "#002948"
  ))

## ---- fig.width=10, fig.height=10---------------------------------------------
# Paint by community
# for each vertex, replace X with the community number

set.seed(200100)

V(net$network_country)$color2 <- rep(NA, length(V(net$network_country)$size))

for (i in 1:length(V(net$network_country)$color2)) {
  com_i <- as.character(com_country$membership[i])

  # if len(com$membership[i]) = 1, append a 0
  if (nchar(com_i) == 1) {
    com_i <- paste0("0", com_i)
  }

  V(net$network_country)$color2[i] <- paste0("Community ", com_i)
}

my_colors <- c(
  "#74c0e2", "#406662", "#549e95", "#8abdb6", "#bcd8af",
  "#a8c380", "#ede788", "#d6c650", "#dc8e7a", "#d05555",
  "#bf3251", "#872a41"
)

ggraph(net$network_country, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size, color = color2)) +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Countries") +
  theme_void() +
  scale_colour_manual(values = my_colors)

## ---- fig.width=10, fig.height=10---------------------------------------------
# Paint by community
# for each vertex, replace X with the community number

set.seed(200100)

V(net$network_product)$color2 <- rep(NA, length(V(net$network_product)$size))

for (i in 1:length(V(net$network_product)$color2)) {
  com_i <- as.character(com_product$membership[i])

  # if len(com$membership[i]) = 1, append a 0
  if (nchar(com_i) == 1) {
    com_i <- paste0("0", com_i)
  }

  V(net$network_product)$color2[i] <- paste0("Community ", com_i)
}

my_colors_2 <- c(
  "#74c0e2", "#406662", "#549e95", "#8abdb6", "#bcd8af",
  "#a8c380", "#ede788", "#d6c650", "#dc8e7a", "#d05555",
  "#bf3251", "#872a41", "#74c0e2", "#406662", "#549e95",
  "#8abdb6", "#bcd8af", "#a8c380", "#ede788", "#d6c650",
  "#dc8e7a", "#d05555", "#bf3251", "#872a41", "#74c0e2",
  "#406662", "#549e95", "#8abdb6", "#bcd8af", "#a8c380",
  "#ede788", "#d6c650", "#dc8e7a", "#d05555", "#bf3251",
  "#872a41", "#74c0e2", "#406662", "#549e95", "#8abdb6",
  "#bcd8af"
)

ggraph(net$network_product, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size, color = color2)) +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void() +
  scale_colour_manual(values = my_colors_2)

## ---- fig.width=10, fig.height=10---------------------------------------------
set.seed(200100)

my_colors_3 <- c(
  "#74c0e2", "#406662", "#549e95", "#8abdb6", "#bcd8af",
  "#a8c380", "#ede788", "#d6c650", "#dc8e7a", "#d05555",
  "#bf3251", "#872a41", "#993f7b", "#7454a6", "#a17cb0",
  "#d1a1bc", "#a1aafb", "#5c57d9", "#1c26b3", "#4d6fd0",
  "#7485aa", "#d3d3d3"
)

com_product <- cluster_fluid_communities(net$network_product, no.of.communities = 22)

V(net$network_product)$color2 <- rep(NA, length(V(net$network_product)$size))

for (i in 1:length(V(net$network_product)$color2)) {
  com_i <- as.character(com_product$membership[i])

  # if len(com$membership[i]) = 1, append a 0
  if (nchar(com_i) == 1) {
    com_i <- paste0("0", com_i)
  }

  V(net$network_product)$color2[i] <- paste0("Community ", com_i)
}

ggraph(net$network_product, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size, color = color2)) +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Products") +
  theme_void() +
  scale_colour_manual(values = my_colors_3)

