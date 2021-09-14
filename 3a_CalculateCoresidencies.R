## Determine dates that relatives coresided
## Ali Galezo

library(parallel)

########################################
## Load data.
########################################

# group membership data
members <- readRDS(file = "Data/Raw/members.Rda")

# list of related pairs
relatives <- readRDS(file = "Data/Processed/opposite_sex_relatives.Rda")

# demographic information (including maturedates)
pedigree <- readRDS(file = "Data/Raw/pedigree.Rda")

########################################
## Remove group 9 (unknown) from members.
########################################

members <- members[which(members$grp != 9), ]

########################################
## Only keep target relatives (no unrelated pairs).
########################################

relatives <- subset(relatives, !(relation %in% c("Unrelated_Conf1", "Unrelated_Conf2", "Unrelated_Conf3")))

########################################
## Group different types of half-avunculars together.
########################################

## Types of half-avunculars
aunt_nephews <- c("AuntNephew_MomMatSib","AuntNephew_MomPatSib","AuntNephew_DadMatSib","AuntNephew_DadPatSib")
uncle_nieces <- c("UncleNiece_MomMatSib","UncleNiece_MomPatSib","UncleNiece_DadMatSib","UncleNiece_DadPatSib")

## Collapse
relatives[which(relatives$relation %in% aunt_nephews), "relation"] <- "HalfAuntNephew"
relatives[which(relatives$relation %in% uncle_nieces), "relation"] <- "HalfUncleNiece"

########################################
## Function to determine dates that relatives were coresident.
########################################

datesCoresident <- function(female, male, lodge, cutoff, members, pedigree){
  #' @param female,male sname (individual's unique identifier)
  #' @param lodge TRUE or FALSE
  #' @param cutoff Valid values: "lt", "te", "rat". "lt" = lifetime; "te" = testicular enlargment; "rat" = rank attainment.
  #' @param members The babase table 'members' parsed out by sname into a list
  #' @param pedigree A pedigree with one row per kid
  
  members_fem <- members[[female]]
  members_mal <- members[[male]]
  
  fem_grps <- unique(members_fem$grp)
  fem_never_lodge <- ifelse(length(fem_grps[which(fem_grps >= 3 & fem_grps < 4)]) == 0, TRUE, FALSE)
  fem_always_lodge <- ifelse(length(fem_grps[which(fem_grps < 3 | fem_grps >= 4)]) == 0, TRUE, FALSE)
  
  mal_grps <- unique(members_mal$grp)
  mal_never_lodge <- ifelse(length(mal_grps[which(mal_grps >= 3 & mal_grps < 4)]) == 0, TRUE, FALSE)
  mal_always_lodge <- ifelse(length(mal_grps[which(mal_grps < 3 | mal_grps >= 4)]) == 0, TRUE, FALSE)
  
  if(lodge == TRUE & fem_never_lodge == TRUE & mal_never_lodge == TRUE) return(NA)
  if(lodge == FALSE & fem_always_lodge == TRUE & mal_always_lodge == TRUE) return(NA)
  
  if(cutoff == "te"){
    fem_cutoff <- pedigree[which(pedigree$kid == female), ]$matdate
    if(is.na(fem_cutoff)) members_fem <- NULL
    if(!is.na(fem_cutoff)) members_fem <- members_fem[members_fem$date >= fem_cutoff, ]
    
    mal_cutoff <- pedigree[which(pedigree$kid == male), ]$matdate
    if(is.na(mal_cutoff)) members_mal <- NULL
    if(!is.na(mal_cutoff)) members_mal <- members_mal[members_mal$date >= mal_cutoff, ]
  }
  
  if(cutoff == "rat"){
    fem_cutoff <- pedigree[which(pedigree$kid == female), ]$matdate
    if(is.na(fem_cutoff)) members_fem <- NULL
    if(!is.na(fem_cutoff)) members_fem <- members_fem[members_fem$date >= fem_cutoff, ]
    
    mal_cutoff <- pedigree[which(pedigree$kid == male), ]$rankdate
    if(is.na(mal_cutoff)) members_mal <- NULL
    if(!is.na(mal_cutoff)) members_mal <- members_mal[members_mal$date >= mal_cutoff, ]
  }
  
  if(lodge == FALSE){
    members_fem <- members_fem[members_fem$grp < 3 | members_fem$grp >= 4, ]
    members_mal <- members_mal[members_mal$grp < 3 | members_fem$grp >= 4, ]
    if (is.null(members_fem) | is.null(members_mal)) return(data.frame(date = as.Date(character()), grp = numeric()))
    if (nrow(members_fem) == 0 | nrow(members_mal) == 0) return(data.frame(date = as.Date(character()), grp = numeric()))
    out <- semi_join(members_fem[2:3], members_mal[2:3], by = c("date","grp"))
  }
  
  if(lodge == TRUE){
    members_fem <- members_fem[members_fem$grp >= 3 & members_fem$grp < 4, ]
    members_mal <- members_mal[members_mal$grp >= 3 & members_fem$grp < 4, ]
    if (is.null(members_fem) | is.null(members_mal)) return(data.frame(date = as.Date(character()), grp = numeric()))
    if (nrow(members_fem) == 0 | nrow(members_mal) == 0) return(data.frame(date = as.Date(character()), grp = numeric()))
    out <- semi_join(members_fem[2:3], members_mal[2:3], by = c("date","grp"))
  }
  
  return(out)
  
}

########################################
## Determine dates that relatives were coresident.
########################################

## Separate members table by sname.
members_parsed <- split(members, members$sname)

## List of coresidency dates between relatives.
coresDates <- list(lt = mcmapply(datesCoresident, female = relatives$female, male = relatives$male, MoreArgs = list(lodge = FALSE, cutoff = "lt", members = members_parsed, pedigree = pedigree), mc.cores = detectCores() - 1),
                   te = mcmapply(datesCoresident, female = relatives$female, male = relatives$male, MoreArgs = list(lodge = FALSE, cutoff = "te", members = members_parsed, pedigree = pedigree), mc.cores = detectCores() - 1),
                   rat = mcmapply(datesCoresident, female = relatives$female, male = relatives$male, MoreArgs = list(lodge = FALSE, cutoff = "rat", members = members_parsed, pedigree = pedigree), mc.cores = detectCores() - 1),
                   lt_lodge = mcmapply(datesCoresident, female = relatives$female, male = relatives$male, MoreArgs = list(lodge = TRUE, cutoff = "lt", members = members_parsed, pedigree = pedigree), mc.cores = detectCores() - 1),
                   te_lodge = mcmapply(datesCoresident, female = relatives$female, male = relatives$male, MoreArgs = list(lodge = TRUE, cutoff = "te", members = members_parsed, pedigree = pedigree), mc.cores = detectCores() - 1),
                   rat_lodge = mcmapply(datesCoresident, female = relatives$female, male = relatives$male, MoreArgs = list(lodge = TRUE, cutoff = "rat", members = members_parsed, pedigree = pedigree), mc.cores = detectCores() - 1))
names(coresDates[["lt"]]) <- paste(relatives$female, relatives$male, sep = "_")
names(coresDates[["te"]]) <- paste(relatives$female, relatives$male, sep = "_")
names(coresDates[["rat"]]) <- paste(relatives$female, relatives$male, sep = "_")
names(coresDates[["lt_lodge"]]) <- paste(relatives$female, relatives$male, sep = "_")
names(coresDates[["te_lodge"]]) <- paste(relatives$female, relatives$male, sep = "_")
names(coresDates[["rat_lodge"]]) <- paste(relatives$female, relatives$male, sep = "_")

########################################
## Data cleanliness checks
########################################

## Check if there are any wild-feeding pairs that also coreside in non-study groups (grp > 4):
checkme <- lapply(coresDates[["lt"]], function(x){
  if(is.data.frame(x) == FALSE) return(FALSE)
  grps <- unique(x$grp)
  wild <- ifelse(sum(grps[grps < 3]) == 0, FALSE, TRUE)
  others <- ifelse(sum(grps[grps >= 4]) == 0, FALSE, TRUE)
  return(ifelse(wild == TRUE & others == TRUE, TRUE, FALSE))
})
checkme <- unlist(checkme)
sum(checkme)
# no cases.

## Check if there are any pairs that exclusively coresided in non-study groups (grp > 4):
checkme <- lapply(coresDates[["lt"]], function(x){
  if(is.data.frame(x) == FALSE) return(FALSE)
  grps <- unique(x$grp)
  return(ifelse(sum(grps[grps >= 4]) == 0, FALSE, TRUE))
})
checkme <- unlist(checkme)
sum(checkme)
# no cases.

## Check if there are any instances where non-study groups (grp > 4) appear in the coresidency tables:
checkme <- lapply(coresDates[["lt"]], function(x){
  if(is.data.frame(x) == FALSE) return(FALSE)
  return(unique(x$grp))
})
checkme <- unlist(checkme)
unique(checkme)
# no cases.

########################################
## Determine length of each coresidency period.
########################################

relatives$cores_lt <- as.numeric(as.character(sapply(coresDates[["lt"]], nrow)))
relatives$cores_te <- as.numeric(as.character(sapply(coresDates[["te"]], nrow)))
relatives$cores_rat <- as.numeric(as.character(sapply(coresDates[["rat"]], nrow)))
relatives$cores_lt_lodge <- as.numeric(as.character(sapply(coresDates[["lt_lodge"]], nrow)))
relatives$cores_te_lodge <- as.numeric(as.character(sapply(coresDates[["te_lodge"]], nrow)))
relatives$cores_rat_lodge <- as.numeric(as.character(sapply(coresDates[["rat_lodge"]], nrow)))
relatives <- relatives %>% ungroup()

########################################
## Determine last date pair were coresident (if ever)
########################################

## Last date ever coresident
relatives$last_date <-
  as.Date(sapply(paste(relatives$female, relatives$male, sep = "_"),
                 function(x){
                   nl_dates <- try(coresDates$lt[[x]]$date, silent = TRUE)
                   lodge_dates <- try(coresDates$lt_lodge[[x]]$date, silent = TRUE)
                   if(inherits(nl_dates, "Date") & inherits(lodge_dates, "Date")) dates <- c(nl_dates, lodge_dates)
                   if(!inherits(nl_dates, "Date")) dates <- lodge_dates
                   if(!inherits(lodge_dates, "Date")) dates <- nl_dates
                   if(length(dates) == 0) return(NA)
                   return(max(dates))
                 })
          , origin = "1970-01-01")

########################################
## Determine whether or not the pairs' coresidency length is censored
########################################

## Coresidency length is uncensored if either individual is dead.
## Otherwise, the coresidency length is right-censored.
relatives$censor <- ifelse(relatives$status_female == 1 | relatives$status_male == 1, "uncensored", "censored")

## Even if either individual is dead, coresidency may still be middle-censored if both animals have unknown group membership at some point
## (for females this must be if her group was dropped)
## i.e. if male-female's group is dropped, then male reappears in a study group later and then dies.
relatives[which(relatives$censor == "uncensored" & relatives$status_female == 3 & relatives$status_male == 1 & relatives$statdate_female < relatives$statdate_male), "censor"] <- "censored"

########################################
## Save output for downstream analysis/plotting.
########################################

## Pull in maturedates for downstream analysis/plotting.
relatives <- merge(x = relatives, y = pedigree[c("kid","matgrp","rankdate")], by.x = "male", by.y = "kid", all.x = TRUE)
relatives <- merge(x = relatives, y = pedigree[c("kid","matgrp","matdate")], by.x = "female", by.y = "kid", all.x = TRUE, suffixes = c("_male","_female"))

## Save table with coresidency lengths for all target related pairs.
saveRDS(relatives, "Data/Processed/relatives_coresidency.Rda")
