## Mate choice model: Subfertility model
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
  filter(relation %in% c("Unrelated","PatSibs","FatherDaughter")) %>%
  filter(lodge_grp == FALSE)

########################################
## Run Bayesian logistic regression.
########################################

## Specify priors.
prior <- normal(location = 0, scale = 2.5)

## Run model.
subfertmodel <- stan_glmer(consort ~ relation * fert_status + female_rank + male_rank + (female_rank:male_rank) + num_males + (male_rank:num_males) + female_age + in_natal_grp + ndays + (1|male) + (1|female),
                           data = consorts,
                           prior = prior,
                           prior_intercept = prior,
                           family = binomial(link = "logit"),
                           iter = 6000)
saveRDS(subfertmodel, "Output/Bayesian Output/subfertmodel.RData")

########################################
## Evaluate model.
########################################

## Visualize posterior distribution chains to check for any convergence issues
plot(subfertmodel, "trace", pars = "(Intercept)", regex_pars = c("relation","rank","num", "age", "natal", "ndays"))

## Look at Gelman and Rubin potential scale reduction statistic Rhat:
## all Rhat values should be below 1.1
summary(subfertmodel)[, "Rhat"]

## Check autocorrelations between samples
plot(subfertmodel, "acf", pars = "(Intercept)", regex_pars = c("relation","rank","num","age", "natal", "ndays"))

########################################
## Model output
########################################

## Model output
summary(subfertmodel)

## Turn into manuscript-ready table
table <-
  summary(subfertmodel) %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  filter(grepl("relation", rowname) | rowname == "(Intercept)" | grepl("rank", rowname) | grepl("fert", rowname) | grepl("num_males", rowname) | grepl("age", rowname) | grepl("natal", rowname) | grepl("ndays", rowname)) %>%
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
                                                "fert_statussubfertility",
                                                "relationFatherDaughter",
                                                "relationPatSibs",
                                                "relationFatherDaughter:fert_statussubfertility",
                                                "relationPatSibs:fert_statussubfertility",
                                                "male_rank",
                                                "num_males",
                                                "male_rank:num_males",
                                                "female_rank",
                                                "female_rank:male_rank",
                                                "female_age",
                                                "in_natal_grpTRUE",
                                                "ndays"))) %>%
  arrange(variable) %>%
  mutate(variable = recode(variable,
                           "(Intercept)" = "intercept",
                           "relationFatherDaughter" = "father-daughter",
                           "relationPatSibs" = "paternal siblings",
                           "fert_statussubfertility" = "adolescent subfertility",
                           "relationFatherDaughter:fert_statussubfertility" = "father-daughter:adolescent subfertility",
                           "relationPatSibs:fert_statussubfertility" = "paternal siblings:adolescent subfertility",
                           "male_rank" = "male dominance rank",
                           "num_males" = "number of adult males in group",
                           "male_rank:num_males" = "male dominance rank:number of adult males in group",
                           "female_rank" = "female dominance rank",
                           "female_rank:male_rank" = "female rank:male rank",
                           "female_age" = "female age",
                           "in_natal_grpTRUE" = "natal male",
                           "ndays" = "number of days male and female shared group"
  )) %>%
  rename(" " = variable,
         "log odds" = log_odds,
         "odds ratio" = odds_ratio)

# group kinship classes horizontally
table %>%
  kable(align = "l") %>%
  add_header_above(c(" " = 3, "90% credible interval" = 2, " " = 1)) %>%
  pack_rows("kinship classes", 3, 4, bold = F, hline_after = F) %>%
  cat(file = "Output/subfertmodel_results.html")

## Number of unique ovulatory windows: 2500
length(unique(consorts$cid))
## Number of unique adult females/males
length(unique(consorts$female)) #204
length(unique(consorts$male)) #231
