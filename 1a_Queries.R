## Queries
## Ali Galezo

library(dplyr)

## Connect to babase.
babase <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         host = "localhost",
                         port = 2222,
                         user = "aag45",
                         dbname = "babase",
                         password = getPass::getPass("Database password"))

## Key query for mate-choice models.
mtdcycles <- collect(tbl(babase, sql("SELECT mtd_cycles.cid
                                           , mtd_cycles.female
                                           , mtd_cycles.seq
                                           , mtd_cycles.female_grp
                                           , CASE WHEN mtd_cycles.female_grp >= 3 THEN TRUE ELSE FALSE END AS lodge_grp
                                           , mtd_cycles.ddate_rnkdate
                                           , members.sname AS male
                                           , ranks_female.rank AS female_rank
                                           , ranks_male.rank AS male_rank
                                           , mtd_cycles.female_age
                                           , biograph.matgrp AS male_natal_grp
                                           , CASE WHEN biograph.matgrp = mtd_cycles.female_grp THEN 1 ELSE 0 END AS in_natal_grp
                                           , COUNT(DISTINCT mtd_cycles_lateral.date) AS ndays
                                           , COUNT(DISTINCT actor_actees.iid) AS nconsorts
                                           , CASE WHEN COUNT(DISTINCT actor_actees.iid) > 0 THEN 1 ELSE 0 END AS consort
                                           FROM (SELECT mtd_cycles.cid
                                                      , mtd_cycles.sname AS female
                                                      , mtd_cycles.seq
                                                      , mtd_cycles.ddate - INTERVAL '1 DAY' AS ddate_m1
                                                      , mtd_cycles.ddate - INTERVAL '2 DAYS' AS ddate_m2
                                                      , mtd_cycles.ddate - INTERVAL '3 DAYS' AS ddate_m3
                                                      , mtd_cycles.ddate - INTERVAL '4 DAYS' AS ddate_m4
                                                      , mtd_cycles.ddate - INTERVAL '5 DAYS' AS ddate_m5
                                                      , rnkdate(mtd_cycles.ddate) AS ddate_rnkdate
                                                      , members.grp AS female_grp
                                                      , (mtd_cycles.ddate - biograph.birth)/365.25 AS female_age
                                                 FROM mtd_cycles
                                                 LEFT JOIN members ON members.date = mtd_cycles.ddate AND members.sname = mtd_cycles.sname
                                                 LEFT JOIN biograph ON biograph.sname = mtd_cycles.sname
                                                 WHERE mtd_cycles.ddate IS NOT NULL) AS mtd_cycles
                                           CROSS JOIN LATERAL (VALUES (mtd_cycles.ddate_m1, 'ddate_m1'),
                                                                      (mtd_cycles.ddate_m2, 'ddate_m2'),
                                                                      (mtd_cycles.ddate_m3, 'ddate_m3'),
                                                                      (mtd_cycles.ddate_m4, 'ddate_m4'),
                                                                      (mtd_cycles.ddate_m5, 'ddate_m5')) AS mtd_cycles_lateral(date, date_type)
                                           LEFT JOIN members ON members.date = mtd_cycles_lateral.date AND members.grp = mtd_cycles.female_grp
                                           INNER JOIN rankdates ON rankdates.sname = members.sname AND rankdates.ranked <= members.date
                                           LEFT JOIN actor_actees ON actor_actees.actor = members.sname
                                                                  AND actor_actees.actee = mtd_cycles.female
                                                                  AND actor_actees.act = 'C'
                                                                  AND actor_actees.date = mtd_cycles_lateral.date
                                           LEFT JOIN ranks AS ranks_female ON ranks_female.sname = mtd_cycles.female AND ranks_female.grp = mtd_cycles.female_grp AND ranks_female.rnkdate = mtd_cycles.ddate_rnkdate AND ranks_female.rnktype = 'ADF'
                                           LEFT JOIN ranks AS ranks_male ON ranks_male.sname = members.sname AND ranks_male.grp = mtd_cycles.female_grp AND ranks_male.rnkdate = mtd_cycles.ddate_rnkdate AND ranks_male.rnktype = 'ADM'
                                           LEFT JOIN biograph ON biograph.sname = members.sname
                                           WHERE mtd_cycles.female_grp != 9
                                           GROUP BY mtd_cycles.cid, mtd_cycles.female, mtd_cycles.seq, mtd_cycles.female_grp, mtd_cycles.ddate_rnkdate, members.sname, ranks_female.rank, ranks_male.rank, mtd_cycles.female_age, biograph.matgrp
                                     ")))
saveRDS(mtdcycles, file = "Data/Raw/mtd_cycles.Rda")

## Query pedigree.
pedigree <- collect(tbl(babase, sql("SELECT biograph.sname AS kid
                                          , parents.mom
                                          , parents.dad
                                          , maternal.mom AS mat_grandmom
                                          , maternal.dad AS mat_granddad
                                          , paternal.mom AS pat_grandmom
                                          , paternal.dad AS pat_granddad
                                          , mat_grandmom.mom AS matmat_ggma
                                          , mat_grandmom.dad AS matmat_ggpa
                                          , mat_granddad.mom AS matpat_ggma
                                          , mat_granddad.dad AS matpat_ggpa
                                          , pat_grandmom.mom AS patmat_ggma
                                          , pat_grandmom.dad AS patmat_ggpa
                                          , pat_granddad.mom AS patpat_ggma
                                          , pat_granddad.dad AS patpat_ggpa
                                          , parents.momgrp
                                          , parents.dadgrp
                                          , biograph.matgrp
                                          , biograph.sex
                                          , biograph.birth
                                          , biograph.bstatus
                                          , biograph.statdate
                                          , biograph.status
                                          , dispersedates.dispersed
                                          , dispersedates.dispconfidence
                                          , maturedates.matured AS matdate
                                          , rankdates.ranked AS rankdate
                                       FROM biograph
                                  LEFT JOIN parents ON parents.kid = biograph.sname
                                  LEFT JOIN parents AS maternal ON maternal.kid = parents.mom
                                  LEFT JOIN parents AS paternal ON paternal.kid = parents.dad
                                  LEFT JOIN parents AS mat_grandmom ON mat_grandmom.kid = maternal.mom
                                  LEFT JOIN parents AS mat_granddad ON mat_granddad.kid = maternal.dad
                                  LEFT JOIN parents AS pat_grandmom ON pat_grandmom.kid = paternal.mom
                                  LEFT JOIN parents AS pat_granddad ON pat_granddad.kid = paternal.dad
                                  LEFT JOIN dispersedates ON dispersedates.sname = biograph.sname
                                  LEFT JOIN maturedates ON maturedates.sname = biograph.sname
                                  LEFT JOIN rankdates ON rankdates.sname = biograph.sname
                                      WHERE biograph.sname IS NOT NULL
                                  ")))
saveRDS(pedigree, file = "Data/Raw/pedigree.Rda")

## Query group membership data.
members <- collect(tbl(babase, sql("SELECT members.sname, members.date, members.grp, members.grpofresidency, members.residency FROM members")))
saveRDS(members, file = "Data/Raw/members.Rda")

## Group history.
grp_history <- collect(tbl(babase, sql("SELECT gid, from_group, impermanent, permanent FROM groups_history ORDER BY gid")))
saveRDS(grp_history, file = "Data/Raw/grp_history.Rda")

## Cycle number of females' first pregnancies.
female_dates <- collect(tbl(babase, sql("SELECT SUBSTRING(pregs.pid, 1, 3) AS sname, cycpoints.date AS first_conception, cycpoints_cycles.seq AS cycle_number
                                        FROM pregs
                                        LEFT JOIN cycpoints ON cycpoints.cpid = pregs.conceive
                                        LEFT JOIN cycpoints_cycles ON cycpoints_cycles.sname = SUBSTRING(pregs.pid, 1, 3)
                                        AND cycpoints_cycles.date = cycpoints.date
                                        AND cycpoints_cycles.series = 1
                                        WHERE parity = 1 AND cycpoints_cycles.seq IS NOT NULL")))
saveRDS(female_dates, file = "Data/Raw/female_first_pregnancies.Rda")

## Query natal dispersal dates.
dispersals <- collect(tbl(babase, sql("SELECT biograph.sname
                                      , biograph.status
                                      , biograph.statdate
                                      , biograph.birth
                                      , CASE WHEN dispersedates.dispersed IS NULL THEN biograph.statdate ELSE dispersedates.dispersed END AS dispersed
                                      , ROUND((CASE WHEN dispersedates.dispersed IS NULL THEN biograph.statdate ELSE dispersedates.dispersed END - biograph.birth)/365.25, 1) AS dispersal_age
                                      , dispersedates.dispconfidence
                                      , CASE WHEN biograph.matgrp >= 3 THEN 'provisioned' ELSE 'wild-feeding' END AS grp_type
                                      , biograph.matgrp
                                      , groups_history.last_reg_census
                                      , CASE WHEN dispersedates.dispersed IS NOT NULL THEN 1 ELSE 0 END AS disp_status
                                      , CASE WHEN dispersedates.dispersed IS NOT NULL THEN 'uncensored'
                                      WHEN status = 0 THEN 'censored-alive'
                                      WHEN status = 1 THEN 'censored-dead'
                                      WHEN status = 2 THEN 'censored-deathordisp'
                                      WHEN status = 3 THEN 'censored-grpdrop'
                                      END AS censor_type
                                      FROM biograph
                                      LEFT JOIN dispersedates ON dispersedates.sname = biograph.sname AND dispersedates.dispconfidence IN (4,3,2)
                                      LEFT JOIN groups_history ON groups_history.gid = biograph.matgrp
                                      WHERE biograph.sname IS NOT NULL
                                      AND biograph.sex = 'M'
                                      AND biograph.matgrp < 4
                                      AND study_grp IS NOT NULL")))
saveRDS(dispersals, file = "Data/Raw/dispersals.Rda")
