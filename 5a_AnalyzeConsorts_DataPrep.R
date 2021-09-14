## Mate choice model data prep
## Ali Galezo

library(dplyr)

########################################
## Load data.
########################################

cycles <- readRDS(file = "Data/Raw/mtd_cycles.Rda")
relatives <- readRDS(file = "Data/Processed/opposite_sex_relatives.Rda")
target_relatives <- readRDS(file = "Data/Processed/target_relatives.Rda")
hscores <- read.csv("Data/Manual/genome_wide_anubis_estimates_for_Ali.csv")

########################################
## Prep data for modeling.
########################################

## Calculate number of adult males in group for each date/grp
cycles <- 
  cycles %>%
  group_by(cid) %>%
  mutate(num_males = n_distinct(male)) %>%
  ungroup()

## Merge relatedness data into consorts list
cycles <- merge(x = cycles,
                y = relatives[c("female","male","relation")],
                by = c("male","female"),
                all.x = TRUE)

## Pairs with relation certainty of 0 (meaning at least 1 individual has at least 1 unknown parent) will have a relation of NA.
## Remove these pairs with low-certainty relatedness.
cycles <-
  cycles %>%
  filter((lodge_grp == TRUE & relation %in% c(target_relatives, "Unrelated_Conf1", "Unrelated_Conf2", "Unrelated_Conf3")) |
           (lodge_grp == FALSE & relation %in% c(target_relatives, "Unrelated_Conf2", "Unrelated_Conf3")))

## Collapse unrelated pairs of different confidences into one category.
cycles[which(cycles$relation %in% c("Unrelated_Conf1", "Unrelated_Conf2", "Unrelated_Conf3")), "relation"] <- "Unrelated"

## Specify types of half-avunculars
aunt_nephews <- c("AuntNephew_MomMatSib","AuntNephew_MomPatSib","AuntNephew_DadMatSib","AuntNephew_DadPatSib")
uncle_nieces <- c("UncleNiece_MomMatSib","UncleNiece_MomPatSib","UncleNiece_DadMatSib","UncleNiece_DadPatSib")

## Collapse types of half-avunculars into single categories
cycles[which(cycles$relation %in% aunt_nephews), "relation"] <- "HalfAuntNephew"
cycles[which(cycles$relation %in% uncle_nieces), "relation"] <- "HalfUncleNiece"

## Set "unrelated" as the reference level
cycles$relation <- factor(cycles$relation)
cycles$relation <- relevel(cycles$relation, ref = "Unrelated")

## Add adolescent subfertility column
cycles$fert_status <- ifelse(cycles$seq >= 9, "fertility", "subfertility")
cycles$fert_status <- as.factor(cycles$fert_status)
cycles$fert_status <- relevel(cycles$fert_status, ref = "fertility")

## Merge in hybrid scores
cycles <- merge(x = cycles,
                y = hscores,
                by.x = "female",
                by.y = "sname",
                all.x = TRUE)
cycles <- merge(x = cycles,
                y = hscores,
                by.x = "male",
                by.y = "sname",
                suffixes = c("_female","_male"),
                all.x = TRUE)

## Calculate assortative mating index
cycles$ami <- pmax(cycles$anubis_admix_male * cycles$anubis_admix_female , (1 - cycles$anubis_admix_male) * (1 - cycles$anubis_admix_female))
## Regress assortative mating index on male anubis score and save residuals.
ami_resid <- lm(ami ~ anubis_admix_male, data = cycles)$residuals
cycles <- merge(cycles, data.frame(ami_resid), by = "row.names", all.x = TRUE)
cycles <- cycles[, -which(names(cycles) == "Row.names")]

## Change "is_natal_male" to be boolean
cycles$in_natal_grp <- as.logical(cycles$in_natal_grp)

########################################
## Save dataset for analysis
########################################

saveRDS(cycles, "Data/Processed/mate_choice_data.Rda")
