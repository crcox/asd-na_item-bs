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
