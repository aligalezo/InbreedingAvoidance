## Assign pedigree relatedness to all possible pairs of animals from a pedigree.
## Ali Galezo

library(parallel)
library(data.table)

options(stringsAsFactors = FALSE)

## Load population pedigree.
pedigree <- readRDS(file = "Data/Raw/pedigree.Rda")
pedigree <- data.table(pedigree)

## List of all sname pairs.
snames <- unique(c(pedigree$kid, pedigree$mom, pedigree$dad))
snames <- snames[!is.na(snames)]
pairs <- as.data.frame(t(combn(snames, 2)))
pairs <- data.table(pairs)

## Pull in sex, parents, grandparents, and great-grandparents.
pairs <- merge(x = pairs,
               y = pedigree,
               by.x = "V1", by.y = "kid",
               all.x = TRUE)
pairs <- merge(x = pairs,
               y = pedigree,
               by.x = "V2", by.y = "kid",
               all.x = TRUE, suffixes = c("_V1","_V2"))

## Count number of shared parents.
pairs[, shared_parents := sum(mom_V1 == mom_V2, dad_V1 == dad_V2, na.rm=TRUE), by=1:NROW(pairs)]
## Count number of NA parents.
pairs[, V1_unknown_parents := sum(is.na(mom_V1), is.na(dad_V1)), by = 1:nrow(pairs)]
pairs[, V2_unknown_parents := sum(is.na(mom_V1), is.na(dad_V2)), by = 1:nrow(pairs)]

## Count number of shared grandparents.
pairs[, shared_grandparents := sum(mat_grandmom_V1 == mat_grandmom_V2 | mat_grandmom_V1 == pat_grandmom_V2,
                                   mat_granddad_V1 == mat_granddad_V2 | mat_granddad_V1 == pat_granddad_V2,
                                   pat_grandmom_V1 == mat_grandmom_V2 | pat_grandmom_V1 == pat_grandmom_V2,
                                   pat_granddad_V1 == mat_granddad_V2 | pat_granddad_V1 == pat_granddad_V2,
                                   na.rm = TRUE), by = 1:NROW(pairs)]
## Count number of NA grandparents.
pairs[, V1_unknown_grandparents := sum(is.na(mat_grandmom_V1), is.na(mat_granddad_V1), is.na(pat_grandmom_V1), is.na(pat_granddad_V1)), by = 1:nrow(pairs)]
pairs[, V2_unknown_grandparents := sum(is.na(mat_grandmom_V2), is.na(mat_granddad_V2), is.na(pat_grandmom_V2), is.na(pat_granddad_V2)), by = 1:nrow(pairs)]

## Count number of shared great-grandparents.
pairs[, shared_greatgrandparents := sum(matmat_ggma_V1 == matmat_ggma_V2 | matmat_ggma_V1 == matpat_ggma_V2 | matmat_ggma_V1 == patmat_ggma_V2 | matmat_ggma_V1 == patpat_ggma_V2,
                                        matmat_ggpa_V1 == matmat_ggpa_V2 | matmat_ggpa_V1 == matpat_ggpa_V2 | matmat_ggpa_V1 == patmat_ggpa_V2 | matmat_ggpa_V1 == patpat_ggpa_V2,
                                        matpat_ggma_V1 == matmat_ggma_V2 | matpat_ggma_V1 == matpat_ggma_V2 | matpat_ggma_V1 == patmat_ggma_V2 | matpat_ggma_V1 == patpat_ggma_V2,
                                        matpat_ggpa_V1 == matmat_ggpa_V2 | matpat_ggpa_V1 == matpat_ggpa_V2 | matpat_ggpa_V1 == patmat_ggpa_V2 | matpat_ggpa_V1 == patpat_ggpa_V2,
                                        patmat_ggma_V1 == matmat_ggma_V2 | patmat_ggma_V1 == matpat_ggma_V2 | patmat_ggma_V1 == patmat_ggma_V2 | patmat_ggma_V1 == patpat_ggma_V2,
                                        patmat_ggpa_V1 == matmat_ggpa_V2 | patmat_ggpa_V1 == matpat_ggpa_V2 | patmat_ggpa_V1 == patmat_ggpa_V2 | patmat_ggpa_V1 == patpat_ggpa_V2,
                                        patpat_ggma_V1 == matmat_ggma_V2 | patpat_ggma_V1 == matpat_ggma_V2 | patpat_ggma_V1 == patmat_ggma_V2 | patpat_ggma_V1 == patpat_ggma_V2,
                                        patpat_ggpa_V1 == matmat_ggpa_V2 | patpat_ggpa_V1 == matpat_ggpa_V2 | patpat_ggpa_V1 == patmat_ggpa_V2 | patpat_ggpa_V1 == patpat_ggpa_V2,
                                        na.rm = TRUE), by = 1:nrow(pairs)]
## Count number of NA great-grandparents.
pairs[, V1_unknown_greatgrandparents := sum(is.na(matmat_ggma_V1), is.na(matmat_ggpa_V1), is.na(matpat_ggma_V1), is.na(matpat_ggpa_V1), is.na(patmat_ggma_V1), is.na(patmat_ggpa_V1), is.na(patpat_ggma_V1), is.na(patpat_ggpa_V1)), by = 1:nrow(pairs)]
pairs[, V2_unknown_greatgrandparents := sum(is.na(matmat_ggma_V2), is.na(matmat_ggpa_V2), is.na(matpat_ggma_V2), is.na(matpat_ggpa_V2), is.na(patmat_ggma_V2), is.na(patmat_ggpa_V2), is.na(patpat_ggma_V2), is.na(patpat_ggpa_V2)), by = 1:nrow(pairs)]

## Add columns for relatives categories.
pairs[, relation := "0"]
pairs[, relation_certainty := "0"]
# Low certainty: pair is definitely at least the assigned relation (e.g. maternal siblings), but may possibly be even more closely related (e.g. full sibs) because some pedigree link is unknown (e.g. unknown father).

## Identify full siblings.
pairs[shared_parents == 2, relation := "FullSibs"]
# Full siblings are always high certainty.
pairs[shared_parents == 2, relation_certainty := "High"]

## Identify half siblings.
pairs[mom_V1 == mom_V2 & relation != "FullSibs", relation := "MatSibs"]
pairs[dad_V1 == dad_V2 & relation != "FullSibs", relation := "PatSibs"]
# If any parents are unknown, make low certainty (as they could potentially be full sibs.)
pairs[relation == "MatSibs" & (V1_unknown_parents + V2_unknown_parents) == 0, relation_certainty := "High"]
pairs[relation == "PatSibs" & (V1_unknown_parents + V2_unknown_parents) == 0, relation_certainty := "High"]
pairs[relation == "MatSibs" & (V1_unknown_parents + V2_unknown_parents) > 0, relation_certainty := "Low"]
pairs[relation == "PatSibs" & (V1_unknown_parents + V2_unknown_parents) > 0, relation_certainty := "Low"]

## Identify 3/4 siblings.
pairs[relation == "MatSibs" & shared_grandparents == 4, relation := "3/4MatSibs(FullCous)"]
pairs[relation == "PatSibs" & shared_grandparents == 4, relation := "3/4PatSibs(FullCous)"]
pairs[relation == "MatSibs" & shared_grandparents == 3, relation := "3/4MatSibs(HalfCous)"]
pairs[relation == "PatSibs" & shared_grandparents == 3, relation := "3/4PatSibs(HalfCous)"]
# For any 3/4 sibs, if any parents are unknown, make Low certainty (as they could be full sibs).
pairs[relation %in% c("3/4MatSibs(FullCous)", "3/4PatSibs(FullCous)", "3/4MatSibs(HalfCous)", "3/4PatSibs(HalfCous)") & (V1_unknown_parents + V2_unknown_parents) > 0, relation_certainty := "Low"]
# If 3/4Sibs(HalfCous) have any unknown grandparents, change to low certainty (as they could potentially share additional grandparents and be 3/4Sibs(FullCous))
pairs[relation %in% c("3/4MatSibs(HalfCous)", "3/4PatSibs(HalfCous)") & (V1_unknown_grandparents + V2_unknown_grandparents) > 0, relation_certainty := "Low"]

## Identify first cousins of various degrees.
pairs[shared_parents == 0 & shared_grandparents == 4, relation := "DoubleFirstCousins"]
pairs[shared_parents == 0 & shared_grandparents == 3, relation := "1.5xFirstCousins"]
pairs[shared_parents == 0 & shared_grandparents == 2, relation := "FirstCousins"]
pairs[shared_parents == 0 & shared_grandparents == 1, relation := "HalfFirstCousins"]
# If any of these have any unknown parents or grandparents, change certainty to Low (as they may share more grandparents and thus be bumped up a level)
pairs[relation %in% c("DoubleFirstCousins","1.5xFirstCousins","FirstCousins","HalfFirstCousins"), relation_certainty := "High"]
pairs[relation %in% c("DoubleFirstCousins","1.5xFirstCousins","FirstCousins","HalfFirstCousins") & (V1_unknown_parents + V2_unknown_parents + V1_unknown_grandparents + V2_unknown_grandparents) > 0, relation_certainty := "Low"]

## Identify second cousins of various degrees.
pairs[shared_parents == 0 & shared_grandparents == 0 & shared_greatgrandparents == 8, relation := "QuadrupleSecondCousins"]
pairs[shared_parents == 0 & shared_grandparents == 0 & shared_greatgrandparents == 7, relation := "3.5xSecondCousins"]
pairs[shared_parents == 0 & shared_grandparents == 0 & shared_greatgrandparents == 6, relation := "TripleSecondCousins"]
pairs[shared_parents == 0 & shared_grandparents == 0 & shared_greatgrandparents == 5, relation := "2.5xSecondCousins"]
pairs[shared_parents == 0 & shared_grandparents == 0 & shared_greatgrandparents == 4, relation := "DoubleSecondCousins"]
# If any of these have any unknown great-grandparents, change certainty to "Low" (as they may share more great-grandparents and thus be bumped up a level)
# If any of these have unknown parents, grandparents, or great-grandparents, change certainty to "Low."
# (unknown parents: could be sibs. unknown grandparents: could be first cousins. unknown great-grandparents: could be higher degree of second cousins.)
pairs[relation %in% c("QuadrupleSecondCousins","3.5xSecondCousins","TripleSecondCousins","2.5xSecondCousins","DoubleSecondCousins"), relation_certainty := "High"]
pairs[relation %in% c("QuadrupleSecondCousins","3.5xSecondCousins","TripleSecondCousins","2.5xSecondCousins","DoubleSecondCousins") & (V1_unknown_parents + V2_unknown_parents + V1_unknown_grandparents + V2_unknown_grandparents + V1_unknown_greatgrandparents + V2_unknown_greatgrandparents) > 0, relation_certainty := "Low"]

## For directional pairs (e.g. parent-offspring, avunculars), add column to identifier which is the "upper-generation" individual
## (e.g. the parent in a parent-offspring pair, the aunt/uncle in an avuncular pair)
pairs[, upper_gen_id := "0"]

## Identify parent-offspring pairs.
pairs[V1 == mom_V2, relation := "MotherChild"]
pairs[V1 == mom_V2, upper_gen_id := "V1"]
pairs[V2 == mom_V1, relation := "MotherChild"]
pairs[V2 == mom_V1, upper_gen_id := "V2"]
pairs[V1 == dad_V2, relation := "FatherChild"]
pairs[V1 == dad_V2, upper_gen_id := "V1"]
pairs[V2 == dad_V1, relation := "FatherChild"]
pairs[V2 == dad_V1, upper_gen_id := "V2"]
# All parent-offspring pairs are high-certainty.
pairs[relation %in% c("MotherChild","FatherChild"), relation_certainty := "High"]

## Identify grandparent-grandoffspring pairs.
pairs[V2 == mat_grandmom_V1 | V2 == mat_granddad_V1 | V2 == pat_grandmom_V1 | V2 == pat_granddad_V1, relation := "GrandparentGrandchild"]
pairs[V2 == mat_grandmom_V1 | V2 == mat_granddad_V1 | V2 == pat_grandmom_V1 | V2 == pat_granddad_V1, upper_gen_id := "V2"]
pairs[V1 == mat_grandmom_V2 | V1 == mat_granddad_V2 | V1 == pat_grandmom_V2 | V1 == pat_granddad_V2, relation := "GrandparentGrandchild"]
pairs[V1 == mat_grandmom_V2 | V1 == mat_granddad_V2 | V1 == pat_grandmom_V2 | V1 == pat_granddad_V2, upper_gen_id := "V1"]
# All grandparent-grandoffspring pairs are high-certainty.
pairs[relation == "GrandparentGrandchild", relation_certainty := "High"]

## Identify great-grandparent-great-grandoffspring pairs.
pairs[V2 == matmat_ggma_V1 | V2 == matmat_ggpa_V1 | V2 == matpat_ggma_V1 | V2 == matpat_ggpa_V1 | V2 == patmat_ggma_V1 | V2 == patmat_ggpa_V1 | V2 == patpat_ggma_V1 | V2 == patpat_ggpa_V1, relation := "GreatgrandparentGreatgrandchild"]
pairs[V2 == matmat_ggma_V1 | V2 == matmat_ggpa_V1 | V2 == matpat_ggma_V1 | V2 == matpat_ggpa_V1 | V2 == patmat_ggma_V1 | V2 == patmat_ggpa_V1 | V2 == patpat_ggma_V1 | V2 == patpat_ggpa_V1, upper_gen_id := "V2"]
pairs[V1 == matmat_ggma_V2 | V1 == matmat_ggpa_V2 | V1 == matpat_ggma_V2 | V1 == matpat_ggpa_V2 | V1 == patmat_ggma_V2 | V1 == patmat_ggpa_V2 | V1 == patpat_ggma_V2 | V1 == patpat_ggpa_V2, relation := "GreatgrandparentGreatgrandchild"]
pairs[V1 == matmat_ggma_V2 | V1 == matmat_ggpa_V2 | V1 == matpat_ggma_V2 | V1 == matpat_ggpa_V2 | V1 == patmat_ggma_V2 | V1 == patmat_ggpa_V2 | V1 == patpat_ggma_V2 | V1 == patpat_ggpa_V2, upper_gen_id := "V1"]
# All grandparent-grandoffspring pairs are high-certainty.
pairs[relation == "GreatgrandparentGreatgrandchild", relation_certainty := "High"]

## Identify avuncular pairs.
## Pull out list of siblings.
sibs <- pairs[relation %in% c("FullSibs","3/4MatSibs(HalfCous)","3/4PatSibs(HalfCous)","MatSibs","PatSibs"),
              c("V1","V2","relation","relation_certainty")]
sibs <- merge(x = sibs, y = pedigree[, c("kid","sex")], by.x = "V1", by.y = "kid", all.x = TRUE)
sibs <- merge(x = sibs, y = pedigree[, c("kid","sex")], by.x = "V2", by.y = "kid", all.x = TRUE, suffixes = c("_V1","_V2"))
pairs_momV1 <- copy(pairs)
setkey(pairs_momV1, mom_V1)
pairs_momV2 <- copy(pairs)
setkey(pairs_momV2, mom_V2)
pairs_dadV1 <- copy(pairs)
setkey(pairs_dadV1, dad_V1)
pairs_dadV2 <- copy(pairs)
setkey(pairs_dadV2, dad_V2)
## Function to identify avuncular pairs based on known siblings.
avunculars <- mclapply(1:nrow(sibs), function(x){
  siba <- sibs[[x, "V1"]]
  sibb <- sibs[[x, "V2"]]
  siba_sex <- sibs[[x, "sex_V1"]]
  sibb_sex <- sibs[[x, "sex_V2"]]
  sibtype <- sibs[[x, "relation"]]
  certainty <- sibs[[x, "relation_certainty"]]
  
  if(sibtype == "FullSibs") avunctype <- "FullSib"
  if(sibtype %in% c("3/4MatSibs(HalfCous)","3/4PatSibs(HalfCous)")) avunctype <- "3/4Sib(HalfCous)"
  if(sibtype %in% c("3/4MatSibs(FullCous)","3/4PatSibs(FullCous)")) avunctype <- "3/4Sib(FullCous)"
  if(sibtype == "MatSibs") avunctype <- "MatSib"
  if(sibtype == "PatSibs") avunctype <- "PatSib"
  
  getKids <- function(name, sex){
    if(sex == "U") return(character())
    if(sex == "F"){
      kids <- pairs_momV1[name, V1]
      kids <- c(kids, pairs_momV2[name, V2])
    }
    if(sex == "M"){
      kids <- pairs_dadV1[name, V1]
      kids <- c(kids, pairs_dadV2[name, V2])
    }
    kids <- unique(kids)
    kids <- kids[!is.na(kids)]
    return(kids)
  }
  
  siba_kids <- getKids(name = siba, sex = siba_sex)
  sibb_kids <- getKids(name = sibb, sex = sibb_sex)
  
  if(length(siba_kids) == 0 & length(sibb_kids) == 0) return(NULL)
  
  getAvuncPairs <- function(sex, kids, sib, avunctype, certainty){
    # @sex the sex of the focal animal
    # @kids offspring of the focal animal (vector)
    # @sib sibling of the focal animal
    # @avunctype type of sibling of the focal animal
    if(sex == "F") relation <- paste0("Avuncular_Mom", avunctype)
    if(sex == "M") relation <- paste0("Avuncular_Dad", avunctype)
    nkids <- length(kids)
    if(nkids == 0) return(NULL)
    if(nkids != 0) return(data.table(V1 = c(kids, rep(sib, nkids)),
                                     V2 = c(rep(sib, nkids), kids),
                                     relation = relation,
                                     relation_certainty = certainty,
                                     upper_gen_id = c(rep("V2", nkids), rep("V1", nkids))
                                     ))
  }
  
  avunc_pairs <- getAvuncPairs(siba_sex, siba_kids, sibb, avunctype, certainty)
  avunc_pairs <- rbind(avunc_pairs,
                       getAvuncPairs(sibb_sex, sibb_kids, siba, avunctype, certainty))
  return(avunc_pairs)
  
})

## Run function to identify avuncular pairs.
avunculars <- do.call("rbind", avunculars)
rm(sibs, pairs_momV1, pairs_momV2, pairs_dadV1, pairs_dadV2)

## Some avuncular pairs are avuncular pairs in two ways, e.g. both Avuncular_MomMatSib and Avuncular_DadPatSib.
## Identify these special cases (DoubleHalfAvuncular and 1.5xAvuncular)
half_avunc_types <- c("Avuncular_MomMatSib","Avuncular_MomPatSib","Avuncular_DadMatSib","Avuncular_DadPatSib")
full_avunc_types <- c("Avuncular_MomFullSib","Avuncular_DadFullSib")
avunculars <-
  avunculars[,
             .(relation = relation,
               relation_certainty = relation_certainty,
               N = .N,
               all_half_avuncs = all(relation %in% half_avunc_types),
               n_half_avuncs = sum(relation %in% half_avunc_types),
               n_full_avuncs = sum(relation %in% full_avunc_types),
               all_high_cert = all(relation_certainty == "High")),
             by = list(V1, V2, upper_gen_id)]
avunculars[N == 2 & all_half_avuncs == TRUE, relation := "DoubleHalfAvuncular"]
avunculars[relation == "DoubleHalfAvuncular" & all_high_cert == TRUE, relation_certainty := "High"]
avunculars[relation == "DoubleHalfAvuncular" & all_high_cert == FALSE, relation_certainty := "Low"]
avunculars[N == 2 & n_half_avuncs == 1 & n_full_avuncs == 1, relation := "1.5xAvuncular"]
avunculars[relation == "1.5xAvuncular" & all_high_cert == TRUE, relation_certainty := "High"]
avunculars[relation == "1.5xAvuncular" & all_high_cert == FALSE, relation_certainty := "Low"]
avunculars[, c("N","all_half_avuncs","n_half_avuncs","n_full_avuncs","all_high_cert") := NULL]
avunculars <- unique(avunculars)

## Pull in all columns from "pairs" into "avunculars" so they can be merged.
setkey(avunculars, V1, V2)
setkey(pairs, V1, V2)
avunculars <- merge(x = avunculars,
                    y = pairs[, !c("upper_gen_id", "relation", "relation_certainty"), with = FALSE],
                    all.x = TRUE)

## Each avuncular pair has two rows depending on ordering of names V1 & V2.
## Only keep the row that corresponds to direction individuals are ordered in in "pairs".
avunculars[, existing_relation := pairs[avunculars][, relation]]
avunculars <- avunculars[!is.na(existing_relation)]
avunculars[, existing_relation := NULL]

## Add avuncular pairs to "pairs".
pairs <- rbind(pairs, avunculars)
rm(avunculars)
## If pairs have more than one row and relation in one row is 0, delete row with relation 0
pairs[, N := .N, by = list(V1, V2)]
pairs <- pairs[!(N > 1 & relation == 0)]
pairs[, N := .N, by = list(V1, V2)]

## Identify first cousins once removed (my first cousin's child)
cousins <- pairs[relation == "FirstCousins" | relation == "QuadrupleSecondCousins"]
cousinsonceremoved <- mclapply(1:nrow(cousins), function(x){
  cousa <- cousins[[x, "V1"]]
  cousb <- cousins[[x, "V2"]]
  coustype <- cousins[[x, "relation"]]
  if(coustype == "FirstCousins") type <- "FirstCousinsOnceRemoved"
  if(coustype == "QuadrupleSecondCousins") type <- "QuadrupleSecondCousinsOnceRemoved"
  
  cousa_kids <- unique(c(pairs[mom_V1 == cousa | dad_V1 == cousa, V1],
                         pairs[mom_V2 == cousa | dad_V2 == cousa, V2]))
  cousb_kids <- unique(c(pairs[mom_V1 == cousb | dad_V1 == cousb, V1],
                         pairs[mom_V2 == cousb | dad_V2 == cousb, V2]))
  if(length(cousa_kids) == 0 & length(cousb_kids) == 0) return(NULL)
  data.table(V1 = c(cousa_kids, cousb_kids,
                    rep(cousb, length(cousa_kids)),
                    rep(cousa, length(cousb_kids))),
             V2 = c(rep(cousb, length(cousa_kids)),
                    rep(cousa, length(cousb_kids)),
                    cousa_kids, cousb_kids),
             relation = type,
             relation_certainty = cousins[[x, "relation_certainty"]],
             upper_gen_id = c(rep("V2", length(cousa_kids) + length(cousb_kids)),
                              rep("V1", length(cousa_kids) + length(cousb_kids))))
})
cousinsonceremoved <- do.call("rbind", cousinsonceremoved)
rm(cousins)

## Some pairs may be first cousins once removed through two different lineages (e.g. are actually "paired first cousins once removed".) Identify them.
cousinsonceremoved <-
  cousinsonceremoved[,
                     .(relation = relation,
                       relation_certainty = relation_certainty,
                       N = .N,
                       all_high_cert = all(relation_certainty == "High")),
                     by = list(V1, V2, upper_gen_id)]
cousinsonceremoved[N == 2, relation := "PairedFirstCousinsOnceRemoved"]
cousinsonceremoved[relation == "PairedFirstCousinsOnceRemoved" & all_high_cert == TRUE, relation_certainty := "High"]
cousinsonceremoved[relation == "PairedFirstCousinsOnceRemoved" & all_high_cert == FALSE, relation_certainty := "Low"]
cousinsonceremoved <- unique(cousinsonceremoved)
cousinsonceremoved[, N := NULL]
cousinsonceremoved[, all_high_cert := NULL]

## Pull in all columns from "pairs" into "cousinsonceremoved" so they can be merged.
setkey(cousinsonceremoved, V1, V2)
setkey(pairs, V1, V2)
cousinsonceremoved <- merge(x = cousinsonceremoved,
                            y = pairs[, !c("upper_gen_id", "relation", "relation_certainty"), with = FALSE],
                            all.x = TRUE)

## Each cousinsonceremoved pair has two rows depending on ordering of names V1 & V2.
## Only keep the row that corresponds to direction individuals are ordered in in "pairs".
cousinsonceremoved[, existing_relation := pairs[cousinsonceremoved][, relation]]
cousinsonceremoved <- cousinsonceremoved[!is.na(existing_relation)]
cousinsonceremoved[, existing_relation := NULL]

## Add cousinsonceremoved pairs to "pairs".
pairs <- rbind(pairs, cousinsonceremoved)
rm(cousinsonceremoved)
## If pairs have more than one row and relation in one row is 0, delete row with relation 0
pairs[, N := .N, by = list(V1, V2)]
pairs <- pairs[!(N > 1 & relation == 0)]
pairs[, N := NULL]

## Any quadruple second cousins once removed to add?
nrow(pairs[relation == "QuadrupleSecondCousins"])
# no.

## Identify unrelated pairs and their certainty depth
pairs[, certainty_depth := ifelse(V1_unknown_greatgrandparents + V2_unknown_greatgrandparents == 0, 3,
                                  ifelse(V1_unknown_grandparents + V1_unknown_grandparents == 0, 2,
                                         ifelse(V1_unknown_parents + V2_unknown_parents == 0, 1, 0)))]
pairs[certainty_depth == 3 & relation == 0, relation := "Unrelated_Conf3"]
pairs[certainty_depth == 2 & relation == 0, relation := "Unrelated_Conf2"]
pairs[certainty_depth == 1 & relation == 0, relation := "Unrelated_Conf1"]
pairs[certainty_depth == 0 & relation == 0, relation := "Unrelated_Conf0"]

## Save list of related and unrelated pairs.
pairs <- as.data.frame(pairs)
saveRDS(pairs, "Data/Processed/relatives.Rda")

