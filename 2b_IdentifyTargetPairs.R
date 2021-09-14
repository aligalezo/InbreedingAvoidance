## Identify pairs that meet inclusion criteria for analyses
## Ali Galezo

library(data.table)

## Load pedigree relatedness of all pairs in population
pairs <- readRDS("Data/Processed/relatives.Rda")
pairs <- data.table(pairs)

## Pull out just opposite-sex pairs
ops <- pairs[sex_V1 %in% c("M","F") & sex_V2 %in% c("M","F")]
ops <- ops[sex_V1 != sex_V2]

## Rename parent-offspring relation names to reflect opposite-sex requirement
ops[relation == "FatherChild", relation := "FatherDaughter"]
ops[relation == "MotherChild", relation := "MotherSon"]

## Rename grandparent-grandchild pairs to reflect sex of higher-generation/lower-generation individual
ops[relation == "GrandparentGrandchild" & upper_gen_id == "V2" & sex_V2 == "M", relation := "GrandfatherGranddaughter"]
ops[relation == "GrandparentGrandchild" & upper_gen_id == "V2" & sex_V2 == "F", relation := "GrandmotherGrandson"]
ops[relation == "GrandparentGrandchild" & upper_gen_id == "V1" & sex_V1 == "M", relation := "GrandfatherGranddaughter"]
ops[relation == "GrandparentGrandchild" & upper_gen_id == "V1" & sex_V1 == "F", relation := "GrandmotherGrandson"]

## Rename greatgrandparent-greatgrandchild pairs to reflect sex of higher-generation/lower-generation individual
ops[relation == "GreatgrandparentGreatgrandchild" & upper_gen_id == "V2" & sex_V2 == "M", relation := "GreatgrandfatherGreatgranddaughter"]
ops[relation == "GreatgrandparentGreatgrandchild" & upper_gen_id == "V2" & sex_V2 == "F", relation := "GreatgrandmotherGreatgrandson"]
ops[relation == "GreatgrandparentGreatgrandchild" & upper_gen_id == "V1" & sex_V1 == "M", relation := "GreatgrandfatherGreatgranddaughter"]
ops[relation == "GreatgrandparentGreatgrandchild" & upper_gen_id == "V1" & sex_V1 == "F", relation := "GreatgrandmotherGreatgrandson"]

## Rename avuncular pairs to reflect sex of higher-generation/lower-generation individual
avunc_types <- c("Avuncular_MomMatSib","Avuncular_MomPatSib","Avuncular_DadMatSib","Avuncular_DadPatSib",
                 "Avuncular_MomFullSib","Avuncular_DadFullSib",
                 "Avuncular_Mom3/4Sib(HalfCous)", "Avuncular_Dad3/4Sib(HalfCous)",
                 "Avuncular_Mom3/4Sib(FullCous)", "Avuncular_Dad3/4Sib(FullCous)",
                 "DoubleHalfAvuncular", "1.5xAvuncular"
                 )
ops[relation %in% avunc_types & ((upper_gen_id == "V2" & sex_V2 == "M")|(upper_gen_id == "V1" & sex_V1 == "M")),
    relation := sub("Avuncular",
                    "UncleNiece",
                    ops[relation %in% avunc_types & ((upper_gen_id == "V2" & sex_V2 == "M")|(upper_gen_id == "V1" & sex_V1 == "M")), relation])
    ]
ops[relation %in% avunc_types & ((upper_gen_id == "V2" & sex_V2 == "F")|(upper_gen_id == "V1" & sex_V1 == "F")),
    relation := sub("Avuncular",
                    "AuntNephew",
                    ops[relation %in% avunc_types & ((upper_gen_id == "V2" & sex_V2 == "F")|(upper_gen_id == "V1" & sex_V1 == "F")), relation])
    ]

## Identify male and female
ops[, male := ifelse(sex_V1 == "M", V1, ifelse(sex_V2 == "M", V2, "ERROR"))]
ops[, female := ifelse(sex_V1 == "F", V1, ifelse(sex_V2 == "F", V2, "ERROR"))]
ops[, statdate_male := ifelse(sex_V1 == "M", statdate_V1, ifelse(sex_V2 == "M", statdate_V2, "ERROR"))]
ops[, statdate_female := ifelse(sex_V1 == "F", statdate_V1, ifelse(sex_V2 == "F", statdate_V2, "ERROR"))]
ops[, status_male := ifelse(sex_V1 == "M", status_V1, ifelse(sex_V2 == "M", status_V2, "ERROR"))]
ops[, status_female := ifelse(sex_V1 == "F", status_V1, ifelse(sex_V2 == "F", status_V2, "ERROR"))]
ops[, bstatus_male := ifelse(sex_V1 == "M", bstatus_V1, ifelse(sex_V2 == "M", bstatus_V2, "ERROR"))]
ops[, bstatus_female := ifelse(sex_V1 == "F", bstatus_V1, ifelse(sex_V2 == "F", bstatus_V2, "ERROR"))]
ops[, birth_male := ifelse(sex_V1 == "M", birth_V1, ifelse(sex_V2 == "M", birth_V2, "ERROR"))]
ops[, birth_female := ifelse(sex_V1 == "F", birth_V1, ifelse(sex_V2 == "F", birth_V2, "ERROR"))]
ops[, dispersed := ifelse(sex_V1 == "M", dispersed_V1, ifelse(sex_V2 == "M", dispersed_V2, "ERROR"))]
ops[, dispconfidence := ifelse(sex_V1 == "M", dispconfidence_V1, ifelse(sex_V2 == "M", dispconfidence_V2, "ERROR"))]

## Save un-trimmed list of pairs to identify inbred animals
saveRDS(ops, "Data/Processed/opposite_sex_relatives_untrimmed.Rda")

## Pull out high-certainty unrelated pairs and high-certainty related pairs from target relative classes
target_relatives <- readRDS("Data/Processed/target_relatives.Rda")
ops <- ops[relation %in% c("Unrelated_Conf1","Unrelated_Conf2","Unrelated_Conf3") | (relation %in% target_relatives & relation_certainty == "High")]

## Only keep relevant columns
ops <- ops[, c("male","female","relation","statdate_female","statdate_male","status_female","status_male","birth_female","birth_male","bstatus_female","bstatus_male","dispersed","dispconfidence")]

## For remaining pairs, identify any multiply-related pairs
setkey(ops, male, female)
multipairs <- ops[.(ops[duplicated(ops, by = key(ops))][, male],
                    ops[duplicated(ops, by = key(ops))][, female])]

## Remove multiply related pairs, as their relatedness coefficients are intermediates.
multipairs <- unique(multipairs[, c("male","female")])
multipairs[, drop := "drop"]
setkey(multipairs, male, female)
ops <- multipairs[ops]
ops <- ops[is.na(drop)]
ops[, drop := NULL]

## Save final list of target related and unrelated pairs.
ops <- as.data.frame(ops)
ops$statdate_female <- as.Date(as.numeric(ops$statdate_female), origin = "1970-01-01")
ops$statdate_male <- as.Date(as.numeric(ops$statdate_male), origin = "1970-01-01")
ops$birth_female <- as.Date(as.numeric(ops$birth_female), origin = "1970-01-01")
ops$birth_male <- as.Date(as.numeric(ops$birth_male), origin = "1970-01-01")
ops$dispersed <- as.Date(as.numeric(ops$dispersed), origin = "1970-01-01")
saveRDS(ops, "Data/Processed/opposite_sex_relatives.Rda")

