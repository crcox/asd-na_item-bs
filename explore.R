library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(boot)

meta <- readRDS("data/cdi-metadata.rds")
items <- read_csv(
    "./item-id-label.csv",
    col_names = c("num_item_id", "label"),
    col_types = list(col_integer(), col_character())
) |>
    mutate(clust_id = 29354226, proc_id = num_item_id - 1) |>
    relocate(clust_id, proc_id)
read_vsoa_bsci <- function(clust_id, proc_id, num_item_id, label) {
    readRDS(file.path(
        "results-20250507",
        "ci_bonf",
        "bs_ci",
        sprintf("%d-%d-%03d-%s.rds", clust_id, proc_id, num_item_id, label)
    ))
}
word_cis <- pmap(items, read_vsoa_bsci, .progress = TRUE)

x <- map(word_cis, function(x) {
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
}, .progress = TRUE)

missing_words <- tibble(
    num_item_id = which(map_lgl(x, ~ {is.na(.x)[1]}))
) |> left_join(select(meta, num_item_id, word), by = "num_item_id")

modeled_words <- tibble(
    num_item_id = which(map_lgl(x, ~ {!is.na(.x)[1]}))
) |> left_join(select(meta, num_item_id, word), by = "num_item_id")

names(x) <- meta$word
x <- x[modeled_words$num_item_id] |>
    bind_rows(.id = "word") |>
    left_join(select(meta, num_item_id, word), by = "word") |>
    relocate(num_item_id)

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

ggsave("summary_ci_bonf-20250507.png", width = 7, height = 5, dpi = 300)

different <- d %>%
    filter(sign(ci_l) == sign(ci_u)) %>%
    split(., sign(.$diff))

different[[2]] <- arrange(different[[2]], desc(diff))


names(different) <- c("later_NA", "later_ASD")

saveRDS(different, file = "split_item_level_differences_bs_ci_bonf-20250507.rds")
saveRDS(d, file = "item_level_differences_bs_ci_bonf-20250507.rds")

readr::write_csv(d, file = "item_level_differences_bs_ci_bonf-20250507.csv")

dold <- readRDS("results/item_level_differences_bs_ci_bonf.rds")
readr::write_csv(dold, file = "item_level_differences_bs_ci_bonf.csv")
