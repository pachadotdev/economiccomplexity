# packages ----

if (!require("pacman")) install.packages("pacman")
p_load(data.table, tidyr, tidytext, dplyr, ggplot2, viridis, ggstance, igraph, ggraph, economiccomplexity)
p_load_gh("dgrtwo/widyr")

# tidy ----

rick_and_morty_subs <- as_tibble(fread("rick_and_morty_subs.csv")) %>%
  mutate(text = iconv(text, to = "ASCII")) %>%
  drop_na()

rick_and_morty_subs_tidy <- rick_and_morty_subs %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

# random ----

rick_rca <- revealed_comparative_advantage(
  rick_and_morty_subs_tidy %>%
    group_by(episode, word) %>%
    summarise(freq = n()) %>%
    filter(freq >= 25),
  country = "episode",
  product = "word",
  value = "freq"
)

rick_complexity <- complexity_measures(rick_rca)

rick_proximity <- proximity(
  rick_rca,
  diversity = rick_complexity$diversity,
  ubiquity = rick_complexity$ubiquity
)

rick_networks <- networks(
  proximity_countries = rick_proximity$proximity_countries,
  proximity_products = rick_proximity$proximity_products
)

set.seed(1733)

rick_networks$network_products %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value), edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Rick and Morty similar words") +
  theme_void()
