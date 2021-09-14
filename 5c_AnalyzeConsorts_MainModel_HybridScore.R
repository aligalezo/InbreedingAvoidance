## Mate choice model: Main model with hybrid scores
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
## Restrict to wild-feeding animals; adult fertility only; male and female have hybrid scores.
########################################

consorts <-
  consorts %>%
  filter(fert_status == "fertility" & lodge_grp == FALSE & !is.na(ami) & !is.na(female_rank) & !is.na(male_rank))
length(unique(consorts$cid))
length(unique(consorts$female))
length(unique(consorts$male))

########################################
## Bayesian logistic regression:
## wild-feeding animals; adult fertility only.
########################################

## Specify priors.
prior <- normal(location = 0, scale = 2.5)

## Run model.
model <- stan_glmer(consort ~ relation + female_rank + male_rank + (female_rank:male_rank) + num_males + (male_rank:num_males) + in_natal_grp + female_age + anubis_admix_female + anubis_admix_male + ami_resid + ndays + (1|male) + (1|female),
                    data = consorts,
                    prior = prior,
                    prior_intercept = prior,
                    family = binomial(link = "logit"),
                    iter = 6000)
saveRDS(model, "Output/Bayesian Output/mainmodel_hybrid.RData")

########################################
## Diagnostics
########################################

## Visualize posterior distribution chains to check for any convergence issues
trace <- plot(model, "trace", pars = "(Intercept)", regex_pars = c("relation","rank","num","natal","age","ndays","admix","ami"))

## Look at Gelman and Rubin potential scale reduction statistic Rhat:
## all Rhat values should be below 1.1
summary(model)[, "Rhat"]

## Check autocorrelations between samples
plot(model, "acf", pars = "(Intercept)", regex_pars = c("relation","rank","num","natal","age","ndays","admix","ami"))

########################################
## Assess whether coefficients differ from model without hybrid score.
########################################

mainmodel <- readRDS("Output/Bayesian Output/mainmodel.RData")

hybrid_coefs <- summary(model, regex_pars = c("relation","rank","num","age","natal"), pars = c("(Intercept)"))[, c("mean","10%","90%")]
main_coefs <- summary(mainmodel, regex_pars = c("relation","rank","num","age","natal"), pars = c("(Intercept)"))[, c("mean","10%","90%")]

hybrid_coefs <- as.data.frame(hybrid_coefs)
main_coefs <- as.data.frame(main_coefs)

hybrid_coefs$model <- "hybrid"
main_coefs$model <- "main"

hybrid_coefs$var <- rownames(hybrid_coefs)
main_coefs$var <- rownames(main_coefs)

compare <- rbind(hybrid_coefs, main_coefs) %>%
  rename(lower = '10%',
         upper = '90%') %>%
  mutate(var = factor(var, levels = rev(c("(Intercept)",
                                          "male_rank",
                                          "female_rank",
                                          "female_rank:male_rank",
                                          "num_males",
                                          "male_rank:num_males",
                                          "female_age",
                                          "in_natal_grpTRUE",
                                          "ndays",
                                          "relationMotherSon",
                                          "relationFatherDaughter",
                                          "relationMatSibs",
                                          "relationPatSibs",
                                          "relationHalfAuntNephew",
                                          "relationHalfUncleNiece",
                                          "relationHalfFirstCousins")))) %>%
  arrange(var) %>%
  mutate(var = recode(var,
                      '(Intercept)' = "intercept",
                      relationFatherDaughter = "father-daughter",
                      relationHalfAuntNephew = "half aunt-nephew",
                      relationHalfFirstCousins = "half first cousins",
                      relationHalfUncleNiece = "half uncle-niece",
                      relationMatSibs = "maternal siblings",
                      relationMotherSon = "mother-son",
                      relationPatSibs = "paternal siblings",
                      male_rank = "male dominance rank",
                      female_rank = "female dominance rank",
                      in_natal_grpTRUE = "natal male",
                      num_males = "number of adult males in group",
                      ndays = "number of days male and female shared group",
                      'male_rank:num_males' = "male dominance rank:number of adult males in group"
  )) %>%
  ggplot(aes(x = mean, xmin = lower, xmax = upper, y = var, color = model, fill = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbarh(size = 0.5, height = 0, position = position_dodge(width = 0.7)) +
  geom_point(size = 1.5, shape = 21, position = position_dodge(width = 0.7)) +
  theme_classic() +
  ylab("") +
  xlab("posterior median estimate of log odds\n\u00B1 90% credible interval")

ggsave(filename = "mainmodel_base_vs_hybrid.tif", plot = compare, device = "tiff", path = "Output", width = 8.4, height = 4.8, units = "in", dpi = 300)

########################################
## Turn into manuscript-ready table
########################################

table <-
  summary(model) %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  filter(grepl("relation", rowname) | rowname == "(Intercept)" | grepl("rank", rowname) | grepl("num_males", rowname) | grepl("natal", rowname) | grepl("age", rowname) | grepl("ndays", rowname) | grepl("admix", rowname) | grepl("ami", rowname)) %>%
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
                                                "ndays",
                                                "anubis_admix_female",
                                                "anubis_admix_male",
                                                "ami_resid"))) %>%
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
                           "ndays" = "number of days male and female shared group",
                           "anubis_admix_female" = "female anubis admixture score",
                           "anubis_admix_male" = "male anubis admixture score",
                           "ami_resid" = "admixture score assortative mating index"
  )) %>%
  rename(" " = variable,
         "log odds" = log_odds,
         "odds ratio" = odds_ratio)

# group kinship classes horizontally
table %>%
  kable(align = "l") %>%
  add_header_above(c(" " = 3, "90% credible interval" = 2, " " = 1)) %>%
  pack_rows("kinship classes", 2, 8, bold = F, hline_after = F) %>%
  cat(file = "Output/mainmodel_hybrid_results.html")

