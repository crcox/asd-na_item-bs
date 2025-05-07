library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(igraph)

m <- readRDS("data/cdi-metadata.rds")
g <- upgrade_graph(readRDS("data/child_net_graph.rds"))
vertex_tbl <- tibble(
    vix = seq_len(vcount(g)),
    word = names(V(g))
) %>%
    left_join(
        m %>% select(num_item_id, word = cue_CoxHae, category)
    )

vix_category <- split(vertex_tbl$vix, vertex_tbl$category)

netstats_categories <- map(vix_category, function(ix, g) {
    gsub <- induced_subgraph(g, ix)
    tibble(
        clust = transitivity(gsub, type = "global"),
        aspl = mean_distance(gsub),
        indegree = median(degree(gsub, mode = "in"))
    )
}, g = g) %>%
    list_rbind()

netstats_categories$category <- names(vix_category)
