## Mate choice model: main model
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

#consorts <- readRDS("Data/Processed/mate_choice_data.Rda")
consorts <- read.csv("Data/Processed/mate_choice_data.csv")

########################################
## Restrict to wild-feeding animals; adult fertility only.
########################################

consorts <-
  consorts %>%
  filter(fert_status == "fertility" & lodge_grp == FALSE)

## Remove rows where male or female rank are missing
consorts <- consorts %>% filter(!is.na(female_rank) & !is.na(male_rank))

########################################
## Bayesian logistic regression:
## wild-feeding animals; adult fertility only.
########################################

## Specify priors.
prior <- normal(location = 0, scale = 2.5)

## Run model.
model <- stan_glmer(consort ~ relation + female_rank + male_rank + (female_rank:male_rank) + num_males + (male_rank:num_males) + in_natal_grp + female_age + ndays + (1|male) + (1|female),
                    data = consorts,
                    prior = prior,
                    prior_intercept = prior,
                    family = binomial(link = "logit"),
                    iter = 6000)
saveRDS(model, "Output/Bayesian Output/mainmodel.RData")

########################################
## Evaluate model.
########################################

## Visualize posterior distribution chains to check for any convergence issues
trace <- plot(model, "trace", pars = "(Intercept)", regex_pars = c("relation","rank","num","natal","age","ndays"))

## Look at Gelman and Rubin potential scale reduction statistic Rhat:
## all Rhat values should be below 1.1
summary(model)[, "Rhat"]

## Check autocorrelations between samples
plot(model, "acf", pars = "(Intercept)", regex_pars = c("relation","rank","num","natal","age","ndays"))

########################################
## Model output/
########################################

## Model output
summary(model)

## Turn into manuscript-ready table
table <-
  summary(model) %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  filter(grepl("relation", rowname) | rowname == "(Intercept)" | grepl("rank", rowname) | grepl("num_males", rowname) | grepl("natal", rowname) | grepl("age", rowname) | grepl("ndays", rowname)) %>%
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
                                                "relationMotherSon",
                                                "relationFatherDaughter",
                                                "relationMatSibs",
                                                "relationPatSibs",
                                                "relationHalfAuntNephew",
                                                "relationHalfUncleNiece",
                                                "relationHalfFirstCousins",
                                                "female_age",
                                                "female_rank",
                                                "male_rank",
                                                "female_rank:male_rank",
                                                "num_males",
                                                "male_rank:num_males",
                                                "in_natal_grpTRUE",
                                                "ndays"))) %>%
  arrange(variable) %>%
  mutate(variable = recode(variable,
                           "(Intercept)" = "intercept",
                           "relationFatherDaughter" = "father-daughter",
                           "relationHalfAuntNephew" = "half-aunt-nephew",
                           "relationHalfFirstCousins" = "half first cousins",
                           "relationHalfUncleNiece" = "half-uncle-niece",
                           "relationMatSibs" = "maternal siblings",
                           "relationMotherSon" = "mother-son",
                           "relationPatSibs" = "paternal siblings",
                           "female_age" = "female age",
                           "female_rank" = "female dominance rank",
                           "female_rank:male_rank" = "female rank:male rank",
                           "male_rank" = "male dominance rank",
                           "num_males" = "number of adult males in group",
                           "male_rank:num_males" = "male dominance rank:number of adult males in group",
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
  pack_rows("kinship classes", 2, 8, bold = F, hline_after = F) %>%
  cat(file = "Output/mainmodel_results.html")

## Number of unique ovulatory windows: 1700
length(unique(consorts$cid))
## Number of unique adult females/males
length(unique(consorts$female))
length(unique(consorts$male))
## Date range
range(consorts$ddate_rnkdate)

