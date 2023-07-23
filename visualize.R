library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(igraph)

d <- readRDS("results/item_level_differences_bs_ci_bonf.rds")
dsplit <- readRDS("results/split_item_level_differences_bs_ci_bonf.rds")
    # filter(na > 0, asd > 0, na < 680, asd < 680, ci_u < 500)

dplot <- d %>%
    mutate(
        g = factor(
            if_else(sign(ci_l) == sign(ci_u), sign(diff), 0),
            levels = c(-1, 1, 0),
            labels = c("earlier in autism", "later in autism", "no difference"))
    )

ggplot(dplot, aes(x = 1:nrow(dplot), y = diff, color = g)) +
    scale_color_manual(values = c("black", "black", "lightgrey")) +
    geom_linerange(aes(ymin = ci_l, ymax = ci_u), linewidth = .7) +
    geom_hline(yintercept = 0) +
    theme_bw(base_size = 18) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none"
    )
ggsave("vsoa_ci-bonf.pdf", width = 7, height = 4, dpi = 300)
m <- readRDS("data/cdi-metadata.rds")
g <- upgrade_graph(readRDS("data/child_net_graph.rds"))

word_netstats <- tibble(
    vid = seq_len(vcount(g)),
    word = names(V(g)),
    clust = transitivity(g, type = "local"),
    indegree = igraph::degree(g, mode = "in"),
    aspl = map_dbl(1:675, ~{
        from <- .x
        to <- seq_len(675)[-.x]
        mean(distances(g, from, to))
    })
) %>% left_join(select(m, num_item_id, word = cue_CoxHae))

dplot_ns <- dplot %>%
    left_join(select(word_netstats, -word)) %>%
    left_join(select(m, num_item_id, category)) %>%
    rename(vsoa_aut = asd, vsoa_non = na)

dplot_ns_bar <- dplot_ns %>%
    filter(g != "no difference") %>%
    pivot_longer(
        cols = c("clust", "indegree", "aspl"),
        names_to = "netstat",
        values_to = "value"
    ) %>%
    group_by(g, netstat) %>%
    summarize(
        x = mean(value, na.rm = TRUE),
        s = sd(value, na.rm = TRUE),
        se = s / n()
    ) %>%
    ungroup() %>%
    droplevels()

ggplot(dplot_ns_bar, aes(x = g, y = x)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = x-se, ymax = x+se), position = position_dodge()) +
    facet_wrap(~netstat, scales = "free_y")


dplot_ns_tt <- dplot_ns %>%
    filter(!is.nan(vid), vsoa_non > 0, vsoa_aut > 0, vsoa_non < 680, vsoa_aut < 680) %>%
    filter(g != "no difference") %>%
    pivot_longer(
        cols = c("indegree", "aspl", "clust"),
        names_to = "netstat",
        values_to = "value"
    ) %>%
    mutate(
        netstat = factor(netstat, levels = c("indegree", "aspl", "clust"))
    )

tmp <- map(levels(dplot_ns_tt$netstat), ~{
    t.test(value ~ g, data = dplot_ns_tt, subset = netstat == .x)
})
names(tmp) <- levels(dplot_ns_tt$netstat)

tt_plot <- map(tmp, function(tt, conf) {
    p <- c((1 - conf) / 2, 1 - ((1 - conf) / 2))
    ci <- qt(p, tt$parameter, ncp = tt$statistic)
    tibble(t0 = tt$statistic, ci_l = ci[1], ci_u = ci[2])
}, conf = 0.95) %>%
    list_rbind(names_to = "netstat")

ggplot(tt_plot, aes(x = netstat, y = t0)) +
    geom_pointrange(aes(ymin = ci_l, ymax = ci_u), size = 1.5, linewidth = 1) +
    geom_hline(yintercept = 0) +
    theme_bw(base_size = 18) +
    theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )

ggsave("netstats-diff_tscale_ci-95_REVISED.pdf", width = 2, height = 4, dpi = 300)

dsplit

dplot_ns_cor <- dplot_ns %>%
    filter(!is.nan(vid), vsoa_non > 0, vsoa_aut > 0, vsoa_non < 680, vsoa_aut < 680)


dplot_cat <- dplot_ns %>%
    mutate(category = factor(category, levels = sort(unique(category)))) %>%
    group_by(g, category, .drop = FALSE) %>%
    tally() %>%
    mutate(p = n / sum(n)) %>%
    ungroup() %>%
    filter(g != "no difference") %>%
    droplevels() %>%
    left_join(count(m, category, name = "total")) %>%
    group_by(category) %>%
    mutate(
        p_total = n / total,
        empty = sum(n) == 0,
        p_total_diff = diff(p_total),
        small = !any(p_total >= .2)
    )

# Factor labels are organized in alpha order
p_cat_earlierautism <- dplot_cat$p_total[dplot_cat$g == "earlier in autism"]
names(p_cat_earlierautism) <- dplot_cat$category[dplot_cat$g == "earlier in autism"]
ix <- order(p_cat_earlierautism, decreasing = TRUE)
dplot_cat$cat_earlierautism <- factor(dplot_cat$category, levels = names(p_cat_earlierautism)[ix])

p_cat_laterautism <- dplot_cat$p_total[dplot_cat$g == "later in autism"]
names(p_cat_laterautism) <- dplot_cat$category[dplot_cat$g == "later in autism"]
ix <- order(p_cat_laterautism, decreasing = TRUE)
dplot_cat$cat_laterautism <- factor(dplot_cat$category, levels = names(p_cat_laterautism)[ix])

ix <- order(p_cat_laterautism - p_cat_earlierautism, decreasing = TRUE)
dplot_cat$cat_diff <- factor(dplot_cat$category, levels = names(p_cat_laterautism)[ix])

    scale_fill_discrete(list("earlier in autism" = "white", "later in autism" = "black"))


dplot_cat$g <- relevel(dplot_cat$g, "later in autism")
plt_pcat <- ggplot(dplot_cat %>% filter(!empty, abs(p_total_diff) > .10), aes(x = cat_diff, y = p_total, fill = g)) +
    geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
    scale_fill_grey() +
    theme_bw(base_size = 14) +
    theme(
        legend.position = "none"
    ) +
    coord_flip()

plt_pdiff <- ggplot(dplot_cat %>% filter(!empty, abs(p_total_diff) > .10), aes(x = cat_diff, y = p, fill = g)) +
    geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
    scale_fill_grey() +
    theme_bw(base_size = 14) +
    theme(
        legend.position = "none"
    ) +
    coord_flip()

plt_ndiff <- ggplot(dplot_cat %>% filter(!empty, abs(p_total_diff) > .10), aes(x = cat_diff, y = n, fill = g)) +
    geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
    scale_fill_grey() +
    theme_bw(base_size = 14) +
    theme(
        legend.position = "none"
    ) +
    coord_flip()


ggsave("vsoa_diff_categories_pcat.pdf", plt_pcat, width = 4, height = 4, units = "in", dpi = 300)
ggsave("vsoa_diff_categories_pdiff.pdf", plt_pdiff, width = 4, height = 4, units = "in", dpi = 300)
ggsave("vsoa_diff_categories_ndiff.pdf", plt_ndiff, width = 4, height = 4, units = "in", dpi = 300)


dplot_ns_lm <- dplot_ns %>%
    filter(!is.na(vid), vsoa_non > 0, vsoa_aut > 0, vsoa_non < 680, vsoa_aut < 680)


mod <- lm(diff ~ clust + indegree + aspl, data = dplot_ns_lm)

summary(mod)
