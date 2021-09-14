## Analyze occurrence of inbred offspring
## Ali Galezo

library(dplyr)

########################################
## Load data.
########################################

relatives <- readRDS(file = "Data/Processed/opposite_sex_relatives_untrimmed.Rda")
relatives <- as.data.frame(relatives)
pedigree <- readRDS(file = "Data/Raw/pedigree.Rda")

inbred <- merge(x = pedigree %>% filter(!is.na(dad) & !is.na(mom)),
                y = relatives[c("female","male","relation")],
                by.x = c("mom","dad"),
                by.y = c("female","male"),
                all.x = TRUE)
# Save version for publication.
write.csv(inbred, "Data/Processed/extended_pedigree.csv")
inbred <- read.csv("Data/Processed/extended_pedigree.csv")

########################################
## Wild-feeding.
########################################

inbred %>%
  filter(matgrp < 3) %>%
  count(relation)
inbred %>%
  filter(matgrp < 3 & relation != "Unrelated_Conf0" & relation != "Unrelated_Conf1" & relation != "Unrelated_Conf2" & relation != "Unrelated_Conf3")

########################################
## Semi-provisioned.
########################################

inbred %>% filter(matgrp >= 3) %>% count(relation)
inbred %>% filter(relation != "Unrelated_Conf0" & relation != "Unrelated_Conf1" & relation != "Unrelated_Conf2" & relation != "Unrelated_Conf3" & matgrp >= 3)

########################################
## Chisquare for wild-feeding vs. semi-provisioned
########################################

inbred$lodge <- ifelse(inbred$matgrp >= 3, TRUE, FALSE)
inbred$inbred <- ifelse(!is.na(inbred$relation) & !(inbred$relation %in% c("Unrelated_Conf0","Unrelated_Conf1","Unrelated_Conf2","Unrelated_Conf3")), TRUE, FALSE)
tbl <- table(inbred$lodge, inbred$inbred)
chisq.test(tbl, simulate.p.value = TRUE)

