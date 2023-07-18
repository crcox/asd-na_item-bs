#!/usr/bin/env Rscript --vanilla --default-packages=methods,utils,stats

args <- commandArgs(trailingOnly = TRUE)

num_item_id <- as.integer(args[1])
niter <- as.integer(args[2])
data_path <- args[3]

print(str(list(num_item_id, niter, data_path)))

library(boot)

decision_boundary <- function(m, crit_p = .5) {
    L <- log(crit_p / (1 - crit_p))
    g <- contrasts(model.frame(m)$group)[, 1]
    b <- coef(m)
    return(-(b["(Intercept)"] + (b["groupASD"] * g) - L) / (b["nproduced"] + (b["nproduced:groupASD"] * g)))
}

bfun <- function(x, ix) {
    m <- glm(
        produced ~ nproduced * group,
        data = x[ix, ],
        family = "binomial"
    )
    d <- decision_boundary(m)
    d <- c("ASD-NA" = diff(d), "NA" = d[1], "ASD" = d[2])
    return(d)
}

d <- readRDS(data_path)
d <- d[d$num_item_id == num_item_id, ]

str(d)

m <- glm(produced ~ nproduced * group, data = d, family = "binomial")

summary(m)

bs <- boot(d, bfun, R = niter, strata = d$group)

str(bs)

bs_ci <- lapply(1:3, function(i) {
    boot.ci(bs, conf = 0.95, type = c("basic", "perc", "bca"), index = i)
})

print(bs_ci)

saveRDS(m, file = sprintf("%03d_glm.rds", num_item_id))
saveRDS(bs, file = sprintf("%03d_bs.rds", num_item_id))
saveRDS(bs_ci, file = sprintf("%03d_bs_ci.rds", num_item_id))
