## In-text summary stats on coresidency.
## Ali Galezo

library(dplyr)

relatives <- readRDS(file = "Data/Processed/relatives_coresidency.Rda")

relatives %>%
  filter(censor == "uncensored") %>%
  filter((birth_male >= birth_female & birth_male <= statdate_female) | (birth_female >= birth_male & birth_female <= statdate_male)) %>%
  filter(is.na(cores_lt_lodge)) %>%
  mutate(coresided = ifelse(cores_rat > 0, TRUE, FALSE)) %>%
  count(relation, coresided) %>%
  group_by(relation) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  filter(coresided == FALSE)

relatives %>%
  filter(censor == "uncensored") %>%
  filter((birth_male >= birth_female & birth_male <= statdate_female) | (birth_female >= birth_male & birth_female <= statdate_male)) %>%
  filter(is.na(cores_lt_lodge)) %>%
  filter(cores_rat > 0) %>%
  mutate(year = ifelse(cores_rat >= 365, TRUE, FALSE)) %>%
  count(year) %>%
  mutate(total = sum(n),
         prop = n/total)

relatives %>%
  filter(censor == "uncensored") %>%
  filter((birth_male >= birth_female & birth_male <= statdate_female) | (birth_female >= birth_male & birth_female <= statdate_male)) %>%
  filter(is.na(cores_lt_lodge)) %>%
  filter(cores_rat > 0) %>%
  mutate(year = ifelse(cores_rat >= 365, TRUE, FALSE)) %>%
  count(year, relation) %>%
  group_by(relation) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  filter(year == FALSE)
