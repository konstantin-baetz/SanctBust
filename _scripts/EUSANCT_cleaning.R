library(readxl)
library(mefa)
library(janitor)
library(dplyr)
library(haven)
#load the sanction sanction data:
EUSANCT <- read_excel("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EUSANCT_Dataset_Dyadic.xls")
names(EUSANCT)

EUSANCT$cname_year <- NULL
EUSANCT$p_polity <- NULL
EUSANCT$startyearEU <- NULL
EUSANCT$startyearUS <- NULL
EUSANCT$startyearUN <- NULL
EUSANCT$startdateEU <- NULL
EUSANCT$startdateUS <- NULL
EUSANCT$startdateUN <- NULL
EUSANCT$link <- NULL
EUSANCT$comments <- NULL
EUSANCT$caseid <- NULL
EUSANCT$combination <- NULL
EUSANCT$sendercombination <- NULL
#work with the sanctions dataset. This is necessary to construct pre/post dummies
#for the pre/post analysis of trade flows.
EUSANCT$lengthEU <- EUSANCT$endyearEU - EUSANCT$impositionEU_year
EUSANCT$lengthUS <- EUSANCT$endyearUS - EUSANCT$impositionUS_year
EUSANCT$lengthUN <- EUSANCT$endyearUN - EUSANCT$impositionUN_year
EUSANCT$durationEU <- EUSANCT$year - EUSANCT$impositionEU_year
EUSANCT$durationUS <- EUSANCT$year - EUSANCT$impositionUS_year
EUSANCT$durationUN <- EUSANCT$year - EUSANCT$impositionUN_year
EUSANCT$durationEU[EUSANCT$durationEU < 0] <- NA
EUSANCT$durationUS[EUSANCT$durationUS < 0] <- NA
EUSANCT$durationUN[EUSANCT$durationUN < 0] <- NA
EUSANCT <- EUSANCT[order(EUSANCT$year),]

#for testing:
#df1 <- select(EUSANCT, sender, ccode, cname, year, ccodecow, sanction_dyad, impositionUS_year, endyearUS, lengthUS, durationUS)
#df1 <- df1[df1$sender == "US",]
#df1 <- df1[df1$ccodecow == 700,]
##[for testing]

df1 <- EUSANCT



df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lag_sanct1 = dplyr::lag(sanction_dyad, order_by = year))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lag_sanct2 = dplyr::lag(lag_sanct1))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lag_sanct3 = dplyr::lag(lag_sanct2))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lag_sanct4 = dplyr::lag(lag_sanct3))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lag_sanct5 = dplyr::lag(lag_sanct4))

df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lead_sanct1 = dplyr::lead(sanction_dyad))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lead_sanct2 = dplyr::lead(lead_sanct1))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lead_sanct3 = dplyr::lead(lead_sanct2))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lead_sanct4 = dplyr::lead(lead_sanct3))
df1 <- df1 %>% group_by(sender, ccode) %>% mutate(lead_sanct5 = dplyr::lead(lead_sanct4))
df1 <- df1 %>% ungroup()

df1$last_sanct <- 0
df1$post_sanct_1 <- 0
df1$post_sanct_2 <- 0
df1$post_sanct_3 <- 0
df1$post_sanct_4 <- 0
df1$post_sanct_5 <- 0
df1$post_sanct_any5 <- 0
df1$post_sanct_1[df1$lag_sanct1 == 1 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_2[df1$lag_sanct2 == 1 & df1$lag_sanct1 == 0 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_3[df1$lag_sanct3 == 1 & df1$lag_sanct2 == 0 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_4[df1$lag_sanct4 == 1 & df1$lag_sanct3 == 0 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_5[df1$lag_sanct5 == 1 & df1$lag_sanct4 == 0 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_any5[df1$lag_sanct1 == 1 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_any5[df1$lag_sanct2 == 1 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_any5[df1$lag_sanct3 == 1 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_any5[df1$lag_sanct4 == 1 & df1$sanction_dyad == 0] <- 1
df1$post_sanct_any5[df1$lag_sanct5 == 1 & df1$sanction_dyad == 0] <- 1


df1$first_sanct <- 0
df1$pre_sanct_1 <- 0
df1$pre_sanct_2 <- 0
df1$pre_sanct_3 <- 0
df1$pre_sanct_4 <- 0
df1$pre_sanct_5 <- 0
df1$pre_sanct_any5 <- 0
df1$pre_sanct_1[df1$lead_sanct1 == 1 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_2[df1$lead_sanct2 == 1 & df1$lead_sanct1 == 0 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_3[df1$lead_sanct3 == 1 & df1$lead_sanct2 == 0 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_4[df1$lead_sanct4 == 1 & df1$lead_sanct3 == 0 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_5[df1$lead_sanct5 == 1 & df1$lead_sanct4 == 0 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_any5[df1$lead_sanct1 == 1 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_any5[df1$lead_sanct2 == 1 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_any5[df1$lead_sanct3 == 1 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_any5[df1$lead_sanct4 == 1 & df1$sanction_dyad == 0] <- 1
df1$pre_sanct_any5[df1$lead_sanct5 == 1 & df1$sanction_dyad == 0] <- 1


df1$first_sanct[df1$lag_sanct1 == 0 & df1$sanction_dyad == 1] <- 1
df1$last_sanct[df1$lead_sanct1 == 0 & df1$sanction_dyad == 1] <- 1

df1$lead_sanct1 <- NULL
df1$lead_sanct2 <- NULL
df1$lead_sanct3 <- NULL
df1$lead_sanct4 <- NULL

df1$lag_sanct1 <- NULL
df1$lag_sanct2 <- NULL
df1$lag_sanct3 <- NULL
df1$lag_sanct4 <- NULL

EUSANCT <- df1
colnames(EUSANCT)[colnames(EUSANCT)=="ccode"] <- "target"
colnames(EUSANCT)[colnames(EUSANCT)=="ccodecow"] <- "targetcow"
colnames(EUSANCT)[colnames(EUSANCT)=="cname"] <- "target_name"
EUSANCT$target <- as.numeric(EUSANCT$target)
EUSANCT$targetcow <- as.numeric(EUSANCT$targetcow)
rm(df1)
#split into UN, EU, US samples:
US_Sanct <- EUSANCT[EUSANCT$sender == "US",]
EU_Sanct <- EUSANCT[EUSANCT$sender == "EU",]
UN_Sanct <- EUSANCT[EUSANCT$sender == "UN",]
save(EU_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct1")
save(US_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct1")
save(UN_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct1")
save(EUSANCT, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EUSANCT_pre_post_dummies" )
rm(EUSANCT, US_Sanct, EU_Sanct, UN_Sanct)