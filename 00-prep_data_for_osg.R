library(dplyr)
library(tidyr)

load("data/ASD_NA_metadata_2023_match.Rdata")
d <- readRDS("data/asd_na-osg-2023_06_30.rds")

ASD_NA_metadata_2023_match %>%
    select(group, subjectkey, interview_age, num_item_id, produced = Produces, nproduced = nProduced) %>%
    group_by(subjectkey) %>%
    summarize(n = sum(produced), m = nproduced[1]) %>%
    filter(m != n) %>%
    print(n = 28)

ASD_NA_metadata_2023_match %>%
    select(group, subjectkey) %>%
    distinct() %>%
    group_by(group) %>%
    count()

cdi <- ASD_NA_metadata_2023_match %>%
    select(group, subjectkey, interview_age, num_item_id, produced = Produces, nproduced = nProduced) %>%
    pivot_wider(id_cols = c(group, subjectkey, interview_age), names_from = num_item_id, values_from = produced, values_fill = FALSE) %>%
    pivot_longer(cols = c(everything(), -group, -subjectkey, -interview_age), names_to = "num_item_id", values_to = "produced") %>%
    group_by(subjectkey) %>%
    mutate(nproduced = sum(produced), num_item_id = readr::parse_number(num_item_id), subjectkey = as.character(subjectkey)) %>%
    ungroup()
nrow(cdi)
(194 + (194 * 5)) * 680

all.equal(names(cdi), names(d))
str(cdi)
str(d)
