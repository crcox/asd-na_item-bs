library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(boot)

meta <- readRDS("data/cdi-metadata.rds")
x <- map_df(1:680, function(i) {
    p <- file.path("results", "ci_bonf", "bs_ci", sprintf("%03d.rds", i))
    x <- readRDS(p)
    d <- expand_grid(
        ci_type = c("basic", "bca", "percentile"),
        variable = c("ASD-NA", "NA", "ASD")
    )
    d$ci_l <- c(
        map_dbl(x, function(ci) {ci$basic[4]}),
        map_dbl(x, function(ci) {ci$bca[4]}),
        map_dbl(x, function(ci) {ci$perc[4]})
    )
    d$ci_u <- c(
        map_dbl(x, function(ci) {ci$basic[5]}),
        map_dbl(x, function(ci) {ci$bca[5]}),
        map_dbl(x, function(ci) {ci$perc[5]})
    )
    d$diff <- rep(map_dbl(x, ~{.$t0}), 3)
    d$na <- x[[2]]$t0
    d$asd <- x[[3]]$t0
    return(d)
}, .id = "num_item_id")

d <- x %>%
    filter(variable == "ASD-NA", ci_type == "bca") %>%
    mutate(num_item_id = as.numeric(num_item_id)) %>%
    arrange(diff) %>%
    left_join(select(meta, num_item_id, word)) %>%
    mutate(fct = factor(word, levels = word))

dplot <- d %>%
    filter(na > 0, asd > 0, na < 680, asd < 680, ci_u < 500)

ggplot(dplot, aes(x = fct, y = diff)) +
    geom_pointrange(aes(ymin = ci_l, ymax = ci_u)) +
    geom_hline(yintercept = 0)

ggsave("summary_ci_bonf.png", width = 7, height = 5, dpi = 300)

different <- d %>%
    filter(sign(ci_l) == sign(ci_u)) %>%
    split(., sign(.$diff))

different[[2]] <- arrange(different[[2]], desc(diff))


names(different) <- c("later_NA", "later_ASD")

saveRDS(different, file = "split_item_level_differences_bs_ci_bonf.rds")
saveRDS(d, file = "item_level_differences_bs_ci_bonf.rds")
