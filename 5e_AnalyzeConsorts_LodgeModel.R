## Mate choice model: Lodge group model
## Ali Galezo

library(rstanarm)
library(dplyr)
library(ggplot2)
library(kableExtra)

########################################
## Set seed for reproducibility.
########################################

set.seed(1234)

########################################
## Load data.
########################################

consorts <- readRDS("Data/Processed/mate_choice_data.Rda")

########################################
## Restrict to unrelated, father-daughter, and patsib pairs.
## Restrict to wild-feeding animals only.
########################################

consorts <-
  consorts %>%
  filter(fert_status == "fertility") %>%
  mutate(relation_new = ifelse(relation == "Unrelated", "Unrelated", "Related"))
consorts$relation_new <- as.factor(consorts$relation_new)
consorts$relation_new <- relevel(consorts$relation_new, ref = "Unrelated")
consorts$lodge_grp <- ifelse(consorts$lodge_grp == TRUE, "semi-provisioned", "wild-feeding")
consorts$lodge_grp <- as.factor(consorts$lodge_grp)
consorts$lodge_grp <- relevel(consorts$lodge_grp, ref = "wild-feeding")

########################################
## Run Bayesian logistic regression.
########################################

## Specify priors.
prior <- normal(location = 0, scale = 2.5)

## Run model.
lodgemodel <- stan_glmer(consort ~ relation_new * lodge_grp + female_rank + male_rank + (female_rank:male_rank) + num_males + (male_rank:num_males) + female_age + ndays + (1|male) + (1|female),
                           data = consorts,
                           prior = prior,
                           prior_intercept = prior,
                           family = binomial(link = "logit"),
                           iter = 6000)
saveRDS(lodgemodel, "Output/Bayesian Output/lodgemodel.RData")

########################################
## Evaluate model.
########################################

## Visualize posterior distribution chains to check for any convergence issues
plot(lodgemodel, "trace", pars = "(Intercept)", regex_pars = c("relation","rank","num","lodge","age","ndays"))

## Look at Gelman and Rubin potential scale reduction statistic Rhat:
## all Rhat values should be below 1.1
summary(lodgemodel)[, "Rhat"]

## Check autocorrelations between samples
plot(lodgemodel, "acf", pars = "(Intercept)", regex_pars = c("relation","rank","num","lodge","age","ndays"))

########################################
## Model output.
########################################

## Model output
summary(lodgemodel)

## Turn into manuscript-ready table
table <-
  summary(lodgemodel) %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  filter(grepl("relation", rowname) | rowname == "(Intercept)" | grepl("rank", rowname) | grepl("lodge", rowname) | grepl("num_males", rowname) | grepl("age", rowname) | grepl("ndays", rowname)) %>%
  rename(log_odds = mean,
         variable = rowname,
         lower = `10%`,
         upper = `90%`) %>%
  mutate(log_odds = round(log_odds, 3),
         sd = round(sd, 3),
         lower = round(lower, 3),
         upper = round(upper, 3),
         odds_ratio = round(exp(log_odds), 3)) %>%
  select(variable, log_odds, sd, lower, upper, odds_ratio) %>%
  mutate(variable = factor(variable, levels = c("(Intercept)",
                                                "relation_newRelated",
                                                "lodge_grpsemi-provisioned",
                                                "relation_newRelated:lodge_grpsemi-provisioned",
                                                "male_rank",
                                                "num_males",
                                                "male_rank:num_males",
                                                "female_rank",
                                                "female_rank:male_rank",
                                                "female_age",
                                                "ndays"))) %>%
  arrange(variable) %>%
  mutate(variable = recode(variable,
                           "(Intercept)" = "intercept",
                           "relation_newRelated" = "related",
                           "lodge_grpsemi-provisioned" = "semi-provisioned",
                           "relation_newRelated:lodge_grpsemi-provisioned" = "related:semi-provisioned",
                           "male_rank" = "male dominance rank",
                           "num_males" = "number of adult males in group",
                           "male_rank:num_males" = "male dominance rank:number of adult males in group",
                           "female_rank" = "female rank",
                           "female_rank:male_rank" = "female rank:male rank",
                           "female_age" = "female age",
                           "ndays" = "number of days male and female shared group"
  )) %>%
  rename(" " = variable,
         "log odds" = log_odds,
         "odds ratio" = odds_ratio)

# group kinship classes horizontally
table %>%
  kable(align = "l") %>%
  add_header_above(c(" " = 3, "90% credible interval" = 2, " " = 1)) %>%
  cat(file = "Output/lodgemodel_results.html")

## Number of unique ovulatory windows: 2096
length(unique(consorts$cid))
length(unique(consorts$female))
length(unique(consorts$male))
