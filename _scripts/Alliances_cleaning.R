library(readxl)
library(mefa)
library(janitor)
library(dplyr)
library(haven)
# alliance data:
alliances <- read_dta("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/COW/alliance_v4.1_by_dyad_yearly.dta")
alliances <- alliances[alliances$year > 1988,]
alliances <- select(alliances, ccode1, ccode2, year, defense, neutrality, nonaggression, entente)

alliances_US_to_3rdctry <- alliances[alliances$ccode1 == 2,]
colnames(alliances_US_to_3rdctry)[colnames(alliances_US_to_3rdctry)=="ccode1"] <- "sender"
colnames(alliances_US_to_3rdctry)[colnames(alliances_US_to_3rdctry)=="ccode2"] <- "thrd_ctry"
colnames(alliances_US_to_3rdctry)[colnames(alliances_US_to_3rdctry)=="defense"] <- "all_def_s3"
colnames(alliances_US_to_3rdctry)[colnames(alliances_US_to_3rdctry)=="neutrality"] <- "all_neut_s3"
colnames(alliances_US_to_3rdctry)[colnames(alliances_US_to_3rdctry)=="nonaggression"] <- "all_nonaggr_s3"
colnames(alliances_US_to_3rdctry)[colnames(alliances_US_to_3rdctry)=="entente"] <- "all_entente_s3"
#sender var is not needed, when merging with US subset, only one sender is inlcuded.
alliances_US_to_3rdctry$sender <- NULL

alliances_target_3rdctry <- alliances
colnames(alliances_target_3rdctry)[colnames(alliances_target_3rdctry)=="ccode1"] <- "thrd_ctry"
colnames(alliances_target_3rdctry)[colnames(alliances_target_3rdctry)=="ccode2"] <- "target"
colnames(alliances_target_3rdctry)[colnames(alliances_target_3rdctry)=="defense"] <- "all_def_3t"
colnames(alliances_target_3rdctry)[colnames(alliances_target_3rdctry)=="neutrality"] <- "all_neut_3t"
colnames(alliances_target_3rdctry)[colnames(alliances_target_3rdctry)=="nonaggression"] <- "all_nonaggr_3t"
colnames(alliances_target_3rdctry)[colnames(alliances_target_3rdctry)=="entente"] <- "all_entente_3t"
save(alliances_US_to_3rdctry, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/COW/alliances_US_to_3rdctry")
save(alliances_target_3rdctry, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/COW/alliances_target_3rdctry")

rm(alliances,alliances_target_3rdctry,alliances_US_to_3rdctry)