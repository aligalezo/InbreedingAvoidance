## Generate Sankey diagram illustrating why relatives often don't coreside as adults
## Ali Galezo

library(dplyr)
library(networkD3)

########################################
## Load data
########################################

relatives <- readRDS(file = "Data/Processed/relatives_coresidency.Rda")
members <- readRDS(file = "Data/Raw/members.Rda")
grp_history <- readRDS(file = "Data/Raw/grp_history.Rda")

########################################
## Identify fission-fusion periods
########################################

offshoots <- grp_history[which(!is.na(grp_history$from_group)), c("gid","from_group","permanent")]
colnames(offshoots) <- c("offshoot_grp","parent_grp","permanent")
offshoots$impermanent <- sapply(offshoots$parent_grp, function(x){
  grp_history[[which(grp_history$gid == x), "impermanent"]]
})
offshoots$impermanent <- as.Date(offshoots$impermanent, origin = "1970-01-01")

########################################
## Clean
########################################

## Only include uncensored pairs with overlapping lifespans
relatives <-
  relatives %>%
  filter(censor == "uncensored") %>%
  filter((birth_male >= birth_female & birth_male <= statdate_female) | (birth_female >= birth_male & birth_female <= statdate_male))

## Add column to populate with reason for not coresiding
relatives$cores_failure <- ""

## Calculate date when both individuals in pair were matured
relatives$both_matured <- pmax(relatives$matdate, relatives$rankdate)

## Hone in on parent-offspring pairs and siblings
relatives <- relatives[which(relatives$relation %in% c("MatSibs","PatSibs","FatherDaughter","MotherSon")), ]

## Wild-feeding only
wild <- relatives[which(is.na(relatives$cores_lt_lodge)), ]

########################################
## Pairs that did coreside as adults
########################################

## Pairs that did coreside post-maturity
wild[which(wild$cores_rat > 0), "cores_failure"] <- "coresided"

########################################
## Pairs that never coresided (even as subadults)
########################################

## Pairs that never coresided, even as subadults:
wild[which(wild$cores_failure == "" & wild$cores_lt == 0), "cores_failure"] <- "never_coreside"

## Matsibs that never coresided even as subadults: if matgrp is different, must be due to fission before their births
wild[which(wild$relation == "MatSibs" & wild$cores_failure == "never_coreside" & wild$matgrp_female != wild$matgrp_male), "cores_failure"] <- "prior_fission"
## Matsibs that never coresided even as subadults: if matgrp is the same and they have overlapping lifespans but they still never coresided, must be because brother dispersed before sister was born
wild[which(wild$relation == "MatSibs" & wild$cores_failure == "never_coreside" & wild$matgrp_female == wild$matgrp_male), "cores_failure"] <- "dispersal"

## Patsibs that never coresided even as subadults: if matgrp is the same and they have overlapping lifespans but they still never coresided, must be because brother dispersed before sister was born
patsibs <- wild[which(wild$relation == "PatSibs" & wild$cores_failure == "never_coreside"), ]
patsibs[which(patsibs$matgrp_female == patsibs$matgrp_male), "cores_failure"] <- "dispersal"
## Patsibs that never coresided even as subadults: if matgrps are different AND they are both fission products of the same parent group,
## and fission happened between their conception dates, must be due to dad's group fissioning
patsibs$female_approx_conception <- patsibs$birth_female - 180
patsibs$male_approx_conception <- patsibs$birth_male - 180
patsibs <- merge(x = patsibs, y = offshoots, by.x = "matgrp_female", by.y = "offshoot_grp", all.x = TRUE)
patsibs <- merge(x = patsibs, y = offshoots[c("offshoot_grp","parent_grp")], by.x = "matgrp_male", by.y = "offshoot_grp", all.x = TRUE, suffixes = c("_female","_male"))
patsibs[which(patsibs$matgrp_female != patsibs$matgrp_male &
                patsibs$parent_grp_female == patsibs$parent_grp_male &
                ((patsibs$female_approx_conception >= patsibs$impermanent & patsibs$female_approx_conception <= patsibs$permanent) | (patsibs$male_approx_conception >= patsibs$impermanent & patsibs$male_approx_conception <= patsibs$permanent))), "cores_failure"] <- "prior_fission"
## Patsibs that never coresided even as subadults: if matgrps are different AND it's not due to fission, must be due to dad dispersing
patsibs[which(patsibs$matgrp_female != patsibs$matgrp_male & patsibs$cores_failure == "never_coreside"), "cores_failure"] <- "prior_dispersal"
## Merge patsibs back into list of target relatives
wild <- merge(x = wild, y = patsibs[c("male","female","cores_failure")], by = c("male","female"), suffixes = c("","_new"), all.x = TRUE)
wild[which(!is.na(wild$cores_failure_new)), "cores_failure"] <- wild[which(!is.na(wild$cores_failure_new)), "cores_failure_new"]
wild$cores_failure_new <- NULL
rm(patsibs)

## Mother-son pairs that never coresided even as subadults: must coreside, so they should not be represented in the "never_coreside" category. Check to make sure.
# wild[which(wild$relation == "MotherSon" & wild$cores_failure == "never_coreside"), ]
# Good.

## Father-daughter pairs that never coresided even as subadults: fission separates mother & father after daughter's conception
fd <- wild[which(wild$relation == "FatherDaughter" & wild$cores_failure == "never_coreside"), ]
fd$daughter_approx_conception <- fd$birth_female - 180
fd <- merge(x = fd, y = offshoots, by.x = "matgrp_female", by.y = "offshoot_grp", all.x = TRUE)
fd[which(fd$daughter_approx_conception >= fd$impermanent & fd$daughter_approx_conception <= fd$permanent), "cores_failure"] <- "fission"
## Father-daughter pairs  that never coresided even as subadults: father disperses before daughter is born
fd[which(fd$cores_failure == "never_coreside"), "cores_failure"] <- "dispersal"
## Merge back into list of target relatives
wild <- merge(x = wild, y = fd[c("male","female","cores_failure")], by = c("male","female"), suffixes = c("","_new"), all.x = TRUE)
wild[which(!is.na(wild$cores_failure_new)), "cores_failure"] <- wild[which(!is.na(wild$cores_failure_new)), "cores_failure_new"]
wild$cores_failure_new <- NULL
rm(fd)

########################################
## Pairs that didn't coreside as adults: death
########################################

## Identify older individual in pair
wild$older <- ifelse(wild$birth_female > wild$birth_male, "male",
                     ifelse(wild$birth_male > wild$birth_female, "female",
                            "same_age"))

## Identify pairs in which their last date coresident is also one animal's death date
wild[which(wild$cores_failure == "" &
             ((wild$last_date == wild$statdate_female & wild$status_female == 1 & wild$older == "female") |
                (wild$last_date == wild$statdate_male & wild$status_male == 1 & wild$older == "male"))), "cores_failure"] <- "death_older"
wild[which(wild$cores_failure == "" &
             ((wild$last_date == wild$statdate_female & wild$status_female == 1 & wild$older == "male") |
                (wild$last_date == wild$statdate_male & wild$status_male == 1 & wild$older == "female"))), "cores_failure"] <- "death_younger"

########################################
## Pairs that didn't coreside as adults: fission
########################################

## Identify pairs that coresided as subadults, but group fissions before they mature and they end up in different groups
ffChecker <- function(x){
  # Identify last group they were sighted together in (the "parent group")
  x$parent_grp <- mapply(function(sname, date){
    members[[which(members$date == date & members$sname == sname), "grp"]]
  },
  sname = x$female,
  date = x$last_date)
  # Identify the first group the male and female were sighted in AFTER the last day they were together (the "offshoot group")
  x$offshoot_grp_female <- mapply(function(sname, date){
    date <- date + 1
    members_subset <- members[which(members$date >= date & members$sname == sname), "grp"]
    if(nrow(members_subset) == 0) return(NA)
    members_subset <- members_subset$grp
    members_subset[!members_subset == 9]
    if(length(members_subset) == 0) return(NA)
    min(members_subset)
  },
  sname = x$female,
  date = x$last_date)
  x$offshoot_grp_male <- mapply(function(sname, date){
    date <- date + 1
    members_subset <- members[which(members$date >= date & members$sname == sname), "grp"]
    if(nrow(members_subset) == 0) return(NA)
    members_subset <- members_subset$grp
    members_subset[!members_subset == 9]
    if(length(members_subset) == 0) return(NA)
    min(members_subset)
  },
  sname = x$male,
  date = x$last_date)
  # Merge in the unstable fission periods for each parent->offshoot group pair (if applicable)
  x <- merge(x = x, y = offshoots,
             by.x = c("parent_grp","offshoot_grp_female"), by.y = c("parent_grp","offshoot_grp"),
             all.x = TRUE)
  x <- merge(x = x, y = offshoots,
             by.x = c("parent_grp","offshoot_grp_male"), by.y = c("parent_grp","offshoot_grp"),
             all.x = TRUE, suffixes = c("_female","_male"))
  # If day after the pair's last date together falls within a relevant unstable fission period, mark their split as fission-related
  x[which((x$last_date + 1 >= x$impermanent_female & x$last_date + 1 <= x$permanent_female) | (x$last_date + 1 >= x$impermanent_male & x$last_date + 1 <= x$permanent_male)), "cores_failure"] <- "fission"
  x <- x[c("male","female","cores_failure")]
  return(x)
}
potential_ff <- wild[which(wild$cores_failure == ""), ]
potential_ff_output <- ffChecker(potential_ff)
wild <- merge(x = wild, y = potential_ff_output, by = c("male","female"), suffixes = c("","_new"), all.x = TRUE)
wild[which(!is.na(wild$cores_failure_new)), "cores_failure"] <- wild[which(!is.na(wild$cores_failure_new)), "cores_failure_new"]
wild$cores_failure_new <- NULL
rm(potential_ff, potential_ff_output)

########################################
## Pairs that didn't coreside as adults: death/dispersals
########################################

## Check for death/dispersals: in which pair's last_date is also the last day one of the individuals has a members' entry
possible_death_disp <- wild[which(wild$cores_failure == ""), ]
ddChecker <- function(male, female, last_date){
  members <- members[which(members$grp != 9), ]
  female_last_date <- max(members[which(members$sname == female), ]$date)
  male_last_date <- max(members[which(members$sname == male), ]$date)
  fem_dd <- ifelse(female_last_date == last_date, TRUE, FALSE)
  mal_dd <- ifelse(male_last_date == last_date, TRUE, FALSE)
  return(ifelse(fem_dd == TRUE & mal_dd == FALSE, "female_dd",
                ifelse(fem_dd == TRUE & mal_dd == TRUE, "both_dd",
                       ifelse(fem_dd == FALSE & mal_dd == TRUE, "male_dd", "neither"))))
  }
possible_death_disp$dd <- mapply(ddChecker, male = possible_death_disp$male, female = possible_death_disp$female, last_date = possible_death_disp$last_date)
## If last date is male's last entry, he either died (if status = 1), OR
possible_death_disp[which(possible_death_disp$dd == "male_dd" & possible_death_disp$status_male == 1 & possible_death_disp$older == "female"), "cores_failure"] <- "death_younger"
possible_death_disp[which(possible_death_disp$dd == "male_dd" & possible_death_disp$status_male == 1 & possible_death_disp$older == "male"), "cores_failure"] <- "death_older"
# he disappeared (status = 2), OR
possible_death_disp[which(possible_death_disp$dd == "male_dd" & possible_death_disp$status_male == 2 & possible_death_disp$older == "female"), "cores_failure"] <- "death_dispersal_younger"
possible_death_disp[which(possible_death_disp$dd == "male_dd" & possible_death_disp$status_male == 2 & possible_death_disp$older == "male"), "cores_failure"] <- "death_dispersal_older"
# he disappeared after group was dropped (status = 3):
possible_death_disp[which(possible_death_disp$dd == "male_dd" & possible_death_disp$status_male == 3 & possible_death_disp$older == "female"), "cores_failure"] <- "death_dispersal_younger"
possible_death_disp[which(possible_death_disp$dd == "male_dd" & possible_death_disp$status_male == 3 & possible_death_disp$older == "male"), "cores_failure"] <- "death_dispersal_older"
## If last date is female's last entry (and her status = 1), she died:
possible_death_disp[which(possible_death_disp$dd == "female_dd" & possible_death_disp$status_female == 1 & possible_death_disp$older == "female"), "cores_failure"] <- "death_older"
possible_death_disp[which(possible_death_disp$dd == "female_dd" & possible_death_disp$status_female == 1 & possible_death_disp$older == "male"), "cores_failure"] <- "death_younger"
## If last date is both male and female's last entry, they both died:
possible_death_disp[which(possible_death_disp$dd == "both_dd"), "cores_failure"] <- "death_older"
## Merge back into main list of pairs:
possible_death_disp <- possible_death_disp[c("male","female","cores_failure")]
wild <- merge(x = wild, y = possible_death_disp, by = c("male","female"), suffixes = c("","_new"), all.x = TRUE)
wild[which(!is.na(wild$cores_failure_new)), "cores_failure"] <- wild[which(!is.na(wild$cores_failure_new)), "cores_failure_new"]
wild$cores_failure_new <- NULL
rm(possible_death_disp)

## Dispersals (male sighted in another group after last date together, and on that subsequent date female isn't dead yet):
possible_disp <- wild[which(wild$cores_failure == ""), ]
possible_disp$cores_failure <- mapply(function(male, female, last_date, status_female, statdate_female){
  
  members <- members[which(members$grp != 9), ]
  # Last group they shared
  shared_grp <- members[which(members$sname == male & members$date == last_date), ]$grp
  # First group female sighted in after split
  female_next_date <- min(members[which(members$sname == female & members$date > last_date), ]$date)
  female_next_grp <- members[which(members$sname == female & members$date == female_next_date), ]$grp
  # First group male sighted in after split (if not sighted again, not a known dispersal)
  male_next_date <- min(members[which(members$sname == male & members$date > last_date), ]$date)
  if(length(male_next_date) == 0) return("")
  male_next_grp <- members[which(members$sname == male & members$date == male_next_date), ]$grp
  
  # If female died before male dispersed to new group, leave cores_failure blank for manual checking
  if(status_female == 1 & statdate_female < male_next_date) return("")
  # If male disperses to a new group (different from group shared with female on last date sighted together), and female isn't dead yet, mark as dispersal
  if(male_next_grp != shared_grp) return("dispersal")
  # If female is the one who ends up in the new group, this is potentially fission-related. Leaves cores_failrue blank for manual checking.
  if(female_next_grp != shared_grp) return("")
  # All other cases: leave cores_failure blank for manual checking
  return("")
  },
  male = possible_disp$male,
  female = possible_disp$female,
  last_date = possible_disp$last_date,
  status_female = possible_disp$status_female,
  statdate_female = possible_disp$statdate_female)

wild <- merge(x = wild, y = possible_disp[c("male","female","cores_failure")], by = c("male","female"), suffixes = c("","_new"), all.x = TRUE)
wild[which(!is.na(wild$cores_failure_new)), "cores_failure"] <- wild[which(!is.na(wild$cores_failure_new)), "cores_failure_new"]
wild$cores_failure_new <- NULL
rm(possible_disp)

########################################
## Pairs that didn't coreside as adults: edge cases
########################################

## Check stragglers manually
stragglers <- read.csv("Data/Raw/coresidency_edge_cases.csv", stringsAsFactors = FALSE)
wild <- merge(x = wild, y = stragglers, by = c("male","female"), suffixes = c("","_new"), all.x = TRUE)
wild[which(!is.na(wild$cores_failure_new)), "cores_failure"] <- wild[which(!is.na(wild$cores_failure_new)), "cores_failure_new"]
wild$cores_failure_new <- NULL

########################################
## Save/load processed data for publication
########################################

#wild %>% select(male, female, relation, cores_failure) %>% write.csv(., "Data/Processed/sankey_data.csv")
wild <- read.csv("Data/Processed/sankey_data.csv")

########################################
## Summary and visualization
########################################

## Sankey diagrams
sankeyPrep <- function(list){

  counts <-
    list %>%
    count(cores_failure) %>%
    bind_rows(summarise(., n = sum(n)) %>%
                mutate(cores_failure = "overlapping_lifespan")) %>%
    bind_rows(filter(., cores_failure != "overlapping_lifespan" & cores_failure != "coresided") %>%
                mutate(grp = ifelse(cores_failure %in% c("prior_dispersal","prior_fission"), "prior_events", "not_prior")) %>%
                group_by(grp) %>%
                summarize(n = sum(n)) %>%
                mutate(cores_failure = grp) %>%
                select(-grp))
  
  links <- data.frame(source = c("overlapping_lifespan","overlapping_lifespan",
                                 "prior_events","prior_events",
                                 "not_prior","not_prior","not_prior","not_prior","not_prior","not_prior",
                                 "overlapping_lifespan"),
                      target = c("prior_events",
                                 "not_prior",
                                 "prior_dispersal",
                                 "prior_fission",
                                 "death_younger",
                                 "death_older",
                                 "dispersal",
                                 "death_dispersal_older",
                                 "death_dispersal_younger",
                                 "fission",
                                 "coresided"))
  
  links <- merge(x = links, y = counts, by.x = "target", by.y = "cores_failure", all.x = TRUE)
  
  labels <- data.frame(name = c("overlapping_lifespan",
                                "prior_events",
                                "not_prior",
                                "prior_dispersal",
                                "prior_fission",
                                "death_younger",
                                "death_older",
                                "dispersal",
                                "death_dispersal_older",
                                "death_dispersal_younger",
                                "fission",
                                "coresided"),
                       longname = c("relatives with overlapping lifespans",
                                    "pre-birth demographic events prevent coresidency",
                                    "post-birth demographic events prevent coresidency",
                                    "dispersal (prior to births)",
                                    "fission (prior to births)",
                                    "death (younger)",
                                    "death (older)",
                                    "dispersal",
                                    "death or dispersal (uncertain)(older)",
                                    "death or dispersal (uncertain)(younger)",
                                    "fission",
                                    "coreside as adults"),
                       color = c("#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#aeadad",
                                 "#4b4b4b"))
  
  links <- merge(x = links, y = labels, by.x = "target", by.y = "name", all.x = TRUE)
  links <- merge(x = links, y = labels, by.x = "source", by.y = "name", all.x = TRUE, suffixes = c("_target","_source"))
  
  desired_order <- c("coresided", "prior_events","prior_fission","prior_dispersal","not_prior","fission","dispersal","death_dispersal","death_younger","death_older")
  
  links <- links %>% mutate(target = factor(target, levels = desired_order)) %>% arrange(target)
  
  links <- links %>% filter(!is.na(n))

  output <- mapply(function(source, target, n){
    paste0(source, " [", n, "] ", target)
    },
    source = links$longname_source,
    target = links$longname_target,
    n = links$n
    )
  
  color_output <- mapply(function(target, color){
    paste0(":", target, " ", color)
    },
    target = links$longname_target,
    color = links$color_target)
  
  return(c(output, ":relatives with overlapping lifespans #000000", color_output))
  
}

fileConn <- file("Output/sankey_data_patsibs.txt")
writeLines(sankeyPrep(wild %>% filter(relation == "PatSibs")), fileConn)
close(fileConn)

fileConn <- file("Output/sankey_data_matsibs.txt")
writeLines(sankeyPrep(wild %>% filter(relation == "MatSibs")), fileConn)
close(fileConn)

fileConn <- file("Output/sankey_data_motherson.txt")
writeLines(sankeyPrep(wild %>% filter(relation == "MotherSon")), fileConn)
close(fileConn)

fileConn <- file("Output/sankey_data_fatherdaughter.txt")
writeLines(sankeyPrep(wild %>% filter(relation == "FatherDaughter")), fileConn)
close(fileConn)
