library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(boot)

meta <- readRDS("data/cdi-metadata.rds")

# Note: We are aware of 65 autistic children for whom data was not
# collected/reported for the first 12 items on the CDI.
#  * Models/VSOAs associated with cluster ID 29354226 included these 65 children
#    when modeling these 12 items, which skewed the results.
#  * Models/VSOAs were rerun for these 12 items, excluding these 65 children.
#    These refit data are associated with cluster ID 29679346.
models_to_load <- read_csv(
    "./item-id-label.csv",
    col_names = c("num_item_id", "label"),
    col_types = list(col_integer(), col_character())
) |>
    mutate(
        clust_id = if_else(num_item_id > 12, 29354226, 29679346), # replace data for first 12 items
        proc_id = num_item_id - 1
    ) |>
    relocate(clust_id, proc_id)

# Again, note that each file is distinguished by a cluster ID and a process ID.
# This, coupled with appropriate documentation, provides information about when
# and under conditions data were generated.
read_vsoa_bsci <- function(clust_id, proc_id, num_item_id, label) {
    readRDS(file.path(
        "results-20250507",
        "ci_bonf",
        "bs_ci",
        sprintf("%d-%d-%03d-%s.rds", clust_id, proc_id, num_item_id, label)
    ))
}
word_cis <- pmap(models_to_load, read_vsoa_bsci, .progress = TRUE)

x <- map(word_cis, function(x) {
    d <- expand_grid(
        ci_type = c("basic", "bca", "percentile"),
        variable = c("ASD-NA", "NA", "ASD")
    )
    d$se <- c(
        map_dbl(x, function(ci) {sd(ci$R)}),
        map_dbl(x, function(ci) {ci$bca[4]}),
        map_dbl(x, function(ci) {ci$perc[4]})
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
