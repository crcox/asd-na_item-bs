library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Note: We are aware of 65 autistic children for whom data was not
# collected/reported for the first 12 items on the CDI.
#  * Models/VSOAs associated with cluster ID 29354226 included these 65 children
#    when modeling these 12 items, which skewed the results.
#  * Models/VSOAs were rerun for these 12 items, excluding these 65 children.
#    These refit data are associated with cluster ID 29679346.
meta <- readRDS("data/cdi-metadata.rds") |>
    as_tibble()
items <- read_csv(
    "./item-id-label.csv",
    col_names = c("num_item_id", "label"),
    col_types = list(col_integer(), col_character())
) |>
    mutate(
        clust_id = if_else(num_item_id > 12, 29354226, 29679346), # replace data for first 12 items
        proc_id = num_item_id - 1
    ) |>
    relocate(clust_id, proc_id)
read_vsoa_glm <- function(clust_id, proc_id, num_item_id, label) {
    readRDS(file.path(
        "results-20250507",
        "ci_bonf",
        "glm",
        sprintf("%d-%d-%03d-%s.rds", clust_id, proc_id, num_item_id, label)
    ))
}
word_models <- pmap(items, read_vsoa_glm, .progress = TRUE)

missing_words <- tibble(
    num_item_id = which(map_lgl(word_models, ~ {is.na(.x)[1]}))
) |> left_join(select(meta, num_item_id, word, by = "num_item_id"))

modeled_words <- tibble(
    num_item_id = which(map_lgl(word_models, ~ {!is.na(.x)[1]}))
) |> left_join(select(meta, num_item_id, word), by = "num_item_id")

names(word_models) <- meta$word

word_models <- word_models[modeled_words$num_item_id]

dnew <- expand_grid(
    group = factor(1:2L, levels = 1:2L, c("NA", "ASD")),
    nproduced = 1:680L
)


prob_list <- map(word_models, ~{
    predict(.x, dnew, type = "response")
})

probs <- array(
    do.call(rbind, prob_list),
    dim = c(length(word_models), 680, 2),
    dimnames = list(
        "word" = names(word_models),
        "vocab_size" = NULL,
        group = c("nonautistic", "autistic")
    )
)

probs_df <- expand_grid(
    vocab_size = 1:680L,
    word = factor(seq_along(word_models), labels = names(word_models))
) |>
    mutate(
        autistic = c(probs[, , 2]),
        nonautistic = c(probs[, , 1])
    )

probs_df_mse <- probs_df |>
    group_by(word) |>
    summarize(
        earlier_by = factor(
            if_else(sum(autistic) > sum(nonautistic), 2, 1),
            levels = 1:2L,
            labels = c("nonautistic", "autistic")
        ),
        mse = mean((autistic - nonautistic)^2),
    ) |>
    left_join(meta |> select(word, in_CoxHae)) |>
    filter(in_CoxHae) |>
    group_by(earlier_by) |>
    mutate(
        rank = rank(max(mse) - mse)
    )

ggplot(probs_df_mse |> mutate(bin = as.factor(if_else(mse > .1, 2, 1))), aes(x = word, y = mse)) +
    geom_point(aes(color = bin)) +
    coord_flip()

tmp <- right_join(
    probs_df,
    probs_df_mse |>
        filter(rank < 7)
)

word_selection <- list(
    autistic = tibble(word = c("skate", "this little piggy", "paint", "hide", "brother", "cut", "pour")),
    nonautistic = tibble(word = c("daddy*", "mommy*", "have", "home", "bib", "peekaboo", "baby"))
) |>
    list_rbind(names_to = "earlier_by")

tmp <- probs_df |>
    right_join(word_selection)

n_distinct(tmp$word)

p_pcurves <- list(
    tmp |>
        pivot_longer(
            c(autistic, nonautistic),
            names_to = "group",
            values_to = "probability"
        ) |>
        filter(earlier_by == "nonautistic") |>
        ggplot(aes(x = vocab_size, y = probability, color = word, linetype = group)) +
            geom_line(show.legend = FALSE) +
            scale_color_brewer(palette = "Set2"),
    tmp |>
        pivot_longer(
            c(autistic, nonautistic),
            names_to = "group",
            values_to = "probability"
        ) |>
        filter(earlier_by == "autistic") |>
        ggplot(aes(x = vocab_size, y = probability, color = word, linetype = group)) +
            geom_line(show.legend = FALSE) +
            scale_color_brewer(palette = "Set1"),
    tmp |>
        filter(earlier_by == "nonautistic") |>
        ggplot(aes(x = vocab_size, y = nonautistic - autistic, color = word)) +
            geom_line(show.legend = FALSE) +
            ylim(c(-.06, 1)) +
            scale_color_brewer(palette = "Set2"),
    tmp |>
        filter(earlier_by == "autistic") |>
        ggplot(aes(x = vocab_size, y = autistic - nonautistic, color = word)) +
            geom_line(show.legend = FALSE) +
            ylim(c(-.06, 1)) +
            scale_color_brewer(palette = "Set1")
)

p_pcurves <- map(p_pcurves, ~{
    .x +
        theme_bw(base_size = 32) +
        theme(
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()
        )
})

p_pcurves_multi <- ggarrange(plotlist = p_pcurves, ncol = 2, nrow = 2)

ggsave(
    "aut-na_prob_curves_fix-20250507.pdf",
    plot = p_pcurves_multi,
    width = 309.03,
    height = 240,
    unit = "mm"
)


# VSOA Autistic vs Non-autistic ----
d <- readRDS("results/item_level_differences_bs_ci_bonf.rds")
d_trim <- d |>
    rename(vsoa_na = na, vsoa_aut = asd) |>
    mutate(
        vsoa_na = if_else(vsoa_na < 1, 1, vsoa_na),
        vsoa_na = if_else(vsoa_na > 680, NA, vsoa_na),
        vsoa_aut = if_else(vsoa_aut < 1, 1, vsoa_aut),
        vsoa_aut = if_else(vsoa_aut > 680, NA, vsoa_aut)
    ) |>
    drop_na()

d_trim_wrank <- list(
    orig = d_trim,
    rank = d_trim |>
        mutate(
            vsoa_na = rank(vsoa_na),
            vsoa_aut = rank(vsoa_aut)
        )
) |>
    list_rbind(names_to = "scale")

cor_methods <- c("pearson", "spearman", "kendall")
names(cor_methods) <- cor_methods
map_dbl(cor_methods, function(df, method) {
    cor(df$vsoa_aut, df$vsoa_na, method = method)
}, df = d_trim)


p_vsoa_aut_na <- ggplot(d_trim_wrank, aes(x = vsoa_na, y = vsoa_aut)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~scale) +
    theme_bw(base_size = 32) +
    theme(panel.grid.minor = element_blank())

ggsave(
    "vsoa_aut_na.pdf",
    plot = p_vsoa_aut_na,
    width = 309.03,
    height = 190.15,
    unit = "mm"
)
cor_methods <- c("pearson", "spearman", "kendall")
names(cor_methods) <- cor_methods
map_dbl(cor_methods, function(df, method) {
    cor(df$vsoa_aut, df$vsoa_na, method = method)
}, df = d_trim)


tmp |>
    filter(earlier_by == "autistic") |>
    ggplot(aes(x = nonautistic, y = autistic, color = word)) +
        geom_line() +
        scale_color_brewer(palette = "Set1", )


# VSOA - AoA ----
vsoa_aoa <- readRDS("data/vsoa_aoa-wordbank_engUS.rds") |>
    tidyr::drop_na()

vsoa_aoa_trimmed <- vsoa_aoa |>
    mutate(
        vsoa = if_else(vsoa <   1,  1, vsoa),
        vsoa = if_else(vsoa > 680, NA, vsoa),
        aoa  = if_else( aoa <  12, 12,  aoa),
        aoa  = if_else( aoa >  30, NA,  aoa)
    ) |>
    drop_na()

vsoa_aoa_trimmed_wrank <- list(
    orig = vsoa_aoa_trimmed,
    rank = mutate(vsoa_aoa_trimmed, across(c(aoa, vsoa), rank))
) |>
    list_rbind(names_to = "scale") |>
    mutate(scale = factor(scale, levels = c("orig", "rank"), labels = c("original scale", "rank scale")))

map_dbl(c("pearson" = "pearson", "spearman" = "spearman", "kendall" = "kendall"), function(df, method) {
    cor(vsoa_aoa$aoa, vsoa_aoa$vsoa, method = method)
}, df = vsoa_aoa)

ggplot(vsoa_aoa_trimmed, aes(x = aoa, y = vsoa)) +
    geom_point() +
    geom_smooth(method = "lm")

p_vsoa_aoa <- ggplot(vsoa_aoa_trimmed_wrank, aes(x = aoa, y = vsoa)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~scale, scales = "free_x") +
    theme_bw(base_size = 32) +
    theme(panel.grid.minor = element_blank())

# A0: 1189 x 841 mm
A0 <- list(width = 1189, height = 841)

(0.38 * A0$width) * .9

ggsave(
    "vsoa_aoa.pdf",
    plot = p_vsoa_aoa,
    width = A0$width * .26,
    height= A0$width * .16,
    unit = "mm"
)


# Model BIC ----
mcomp <- tibble(
    g = c("autistic", "autistic", "autistic", "autistic", "autistic", "non-autistic", "non-autistic", "non-autistic", "non-autistic", "non-autistic"),
    m0 = c("baseline", "baseline", "baseline", "Acq.", "LOA", "baseline", "baseline", "baseline", "Acq.", "LOA"),
    m1 = c("Att.", "Acq.", "LOA", "Acq. + LOA", "LOA + Acq.", "Att.", "Acq.", "LOA", "Acq. + LOA", "LOA + Acq."),
    bic = c(-9.72, 59.89, 28.82, 2.16, 33.23, -11.06, 64.36, 28.77, -9.46, 26.14),
    p = c(.224, .001, .001, .001, .001, .444, .001, .001, .199, .001)
)

ggplot(mcomp, aes(x = interaction(m1, m0, sep = " - "), y = bic, fill = g)) +
    geom_bar(stat = "identity", position = position_dodge())
