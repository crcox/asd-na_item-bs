library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(boot)

meta <- readRDS("data/cdi-metadata.rds")

# !!! CRITICAL NOTE (16 May 2025) !!!
# The data modeled to obtain VSOA estimates used numeric item IDs derived from
# NDAR data, while the lab convention is to use the WordBank numeric item ID.
# Special care is needed when reading from `results-20250507`.
wb_to_ndar <- readRDS("data/wb-to-ndar.rds")
id_key <- bind_rows(
    "ASD" = select(wb_to_ndar, num_item_id = ndar_item_id, word),
    "NA" = select(wb_to_ndar, num_item_id = wb_item_id, word),
    .id = "group"
)


# !!! CRITICAL NOTE (19 May 2025) !!!
# We realized that that problem identified on 16 May required rerunning several
# models and fixing the indexes on the front end. We have standardized on
# WordBank IDs.


# !!! CRITICAL NOTE (20 May 2025) !!!
# Models were rerun, and data seems to be correct

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
        clust_id = 32251163,
        proc_id = num_item_id - 1
    ) |>
    relocate(clust_id, proc_id)

# Again, note that each file is distinguished by a cluster ID and a process ID.
# This, coupled with appropriate documentation, provides information about when
# and under conditions data were generated.
read_vsoa_bsci <- function(clust_id, proc_id, num_item_id, label) {
    readRDS(file.path(
        "results-20250520",
        "ci_bonf",
        "bs_ci",
        sprintf("%d-%d-%03d-%s.rds", clust_id, proc_id, num_item_id, label)
    ))
}
word_cis <- pmap(
    models_to_load,
    read_vsoa_bsci,
    .progress = TRUE
)

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

#missing_words <- tibble(
#    num_item_id = which(map_lgl(x, ~ {is.na(.x)[1]}))
#) |> left_join(select(id_key, num_item_id, word), by = c("group", "num_item_id"))
#
#modeled_words <- tibble(
#    num_item_id = which(map_lgl(x, ~ {!is.na(.x)[1]}))
#) |> left_join(select(id_key, num_item_id, word), by = c("group", "ndar_item_id"))


names(x) <- models_to_load |>
    left_join(select(meta, num_item_id, word), by = "num_item_id") |>
    pull(word)

# In this pipeline, we would make the switch from NDAR item IDs to WB item IDs being
# the principle index; `wb_item_id` becomes `num_item_id`.
df_vsoa <- x |>
    bind_rows(.id = "word") |>
    left_join(select(meta, word, num_item_id), by = "word") |>
    relocate(num_item_id) |>
    mutate(
        num_item_id = as.integer(num_item_id),
        word = factor(num_item_id, meta$num_item_id, meta$word),
        ci_type = factor(ci_type, c("basic", "bca", "percentile")),
        group = factor(variable, c("ASD-NA", "NA", "ASD"))
    ) |>
    select(-na, -asd, -variable) |>
    rename(vsoa = diff) |>
    relocate(group, vsoa, .after = word) |>
    arrange(num_item_id, group, ci_type) |>
    filter(ci_type == "bca")

df_vsoa_diff <- df_vsoa |>
    pivot_wider(
        id_cols = c(num_item_id, word, ci_type),
        names_from = group,
        values_from = c(vsoa, ci_l, ci_u)
    ) |>
    rename_with(~ sub("ASD-NA", "diff", .x), ends_with("ASD-NA")) |>
    select(-ci_l_ASD, -ci_u_ASD, -ci_l_NA, -ci_u_NA)


saveRDS(df_vsoa, "data/vsoa-autistic-nonautistic-ndar-id-fix-remodel-v2.rds")
saveRDS(df_vsoa_diff, "data/vsoa-autistic-nonautistic-diff-ndar-id-fix-remodel-v2.rds")

write_csv(df_vsoa_diff, file = "data/vsoa-autistic-nonautistic-diff-ndar-id-fix-remodel-v2.csv")
