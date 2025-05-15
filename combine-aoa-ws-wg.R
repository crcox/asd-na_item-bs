library(dplyr)
library(purrr)
library(tidyr)

x <- bind_rows(
    WG = readRDS("data/CDI_English (American)_WG_14May2025_AoA.rds"),
    WS = readRDS("data/CDI_English (American)_WS_14May2025_AoA.rds"),
    .id = "form"
) |>
    mutate(form = as.factor(form)) |>
    pivot_wider(
        id_cols = c(definition, category, lexical_category),
        names_from = form,
        values_from = aoa_glm
    )

filter(x, is.na(WS), !is.na(WG))
filter(x, !is.na(WS), !is.na(WG))
