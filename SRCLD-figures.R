library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

meta <- readRDS("data/cdi-metadata.rds")
word_models <- map(seq_len(680), ~{
    readRDS(file.path("results", "ci_bonf", "glm", sprintf("%03d.rds", .x)))
})

names(word_models) <- meta$word

dnew <- expand_grid(
    group = factor(1:2L, levels = 1:2L, c("NA", "ASD")),
    nproduced = 1:680L
)


prob_list <- map(word_models, ~{
    predict(.x, dnew, type = "response")
})

probs <- array(
    do.call(rbind, prob_list),
    dim = c(680, 680, 2),
    dimnames = list(
        "word" = names(word_models),
        "vocab_size" = NULL,
        group = c("nonautistic", "autistic")
    )
)

probs_df <- expand_grid(
    vocab_size = 1:680L,
    word = factor(1:680L, labels = names(word_models))
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
n_distinct(tmp$word)

tmp |>
    pivot_longer(
        c(autistic, nonautistic),
        names_to = "group",
        values_to = "probability"
    ) |>
    filter(earlier_by == "autistic") |>
    ggplot(aes(x = vocab_size, y = probability, color = word, linetype = group)) +
        geom_line() +
        scale_color_brewer(palette = "Set2")


tmp |>
    pivot_longer(
        c(autistic, nonautistic),
        names_to = "group",
        values_to = "probability"
    ) |>
    filter(earlier_by == "nonautistic") |>
    ggplot(aes(x = vocab_size, y = probability, color = word, linetype = group)) +
        geom_line() +
        scale_color_brewer(palette = "Set1")


tmp |>
    filter(earlier_by == "nonautistic") |>
    ggplot(aes(x = nonautistic, y = autistic, color = word)) +
        geom_line() +
        scale_color_brewer(palette = "Set2")

tmp |>
    filter(earlier_by == "autistic") |>
    ggplot(aes(x = nonautistic, y = autistic, color = word)) +
        geom_line() +
        scale_color_brewer(palette = "Set1", )


# VSOA - AoA
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

map_dbl(c("pearson" = "pearson", "spearman" = "spearman", "kendall" = "kendall"), function(df, method) {
    cor(df$aoa, df$vsoa, method = method)
}, df = vsoa_aoa_trimmed)

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
