#UN sample merging
#
# ATTENTION:
# do not run alone
# only in merge.R superscript
#############################

load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct1")
UN1900 <- UN_Sanct[UN_Sanct$year < "2000",]
UN2000 <- UN_Sanct[UN_Sanct$year >= "2000",]
UN1900_2<- left_join(UN1900, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))
df <- UN1900_2[!is.na(UN1900_2$year),]
colnames(df)[colnames(df)=="state_i"] <- "target_name_2"
colnames(df)[colnames(df)=="state_j"] <- "thrd_ctry_name"
colnames(df)[colnames(df)=="ccodecow"] <- "ccode_t"
colnames(df)[colnames(df)=="ccode_j"] <- "ccode_3"
colnames(df)[colnames(df)=="Flow_i"] <- "Flow_t3"
colnames(df)[colnames(df)=="Flow_j"] <- "Flow_3t"
colnames(df)[colnames(df)=="logFlow_i"] <- "log_Flow_t3"
colnames(df)[colnames(df)=="logFlow_j"] <- "log_Flow_3t"
colnames(df)[colnames(df)=="GDP_i"] <- "GDP_t"
colnames(df)[colnames(df)=="GDP_j"] <- "GDP_3"
colnames(df)[colnames(df)=="logGDP_i"] <- "logGDP_t"
colnames(df)[colnames(df)=="logGDP_j"] <- "logGDP_3"
colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_t3"
colnames(df)[colnames(df) == "logdistance"] <- "logdistance_t3"
colnames(df)[colnames(df)=="Pop_i"] <- "Pop_t"
colnames(df)[colnames(df)=="Pop_j"] <- "Pop_3"
UN1900_2 <- df


UN2000_2<- left_join(UN2000, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))
df <- UN2000_2[!is.na(UN2000_2$year),]
colnames(df)[colnames(df)=="state_i"] <- "target_name_2"
colnames(df)[colnames(df)=="state_j"] <- "thrd_ctry_name"
colnames(df)[colnames(df)=="ccodecow"] <- "ccode_t"
colnames(df)[colnames(df)=="ccode_j"] <- "ccode_3"
colnames(df)[colnames(df)=="Flow_i"] <- "Flow_t3"
colnames(df)[colnames(df)=="Flow_j"] <- "Flow_3t"
colnames(df)[colnames(df)=="logFlow_i"] <- "log_Flow_t3"
colnames(df)[colnames(df)=="logFlow_j"] <- "log_Flow_3t"
colnames(df)[colnames(df)=="GDP_i"] <- "GDP_t"
colnames(df)[colnames(df)=="GDP_j"] <- "GDP_3"
colnames(df)[colnames(df)=="logGDP_i"] <- "logGDP_t"
colnames(df)[colnames(df)=="logGDP_j"] <- "logGDP_3"
colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_t3"
colnames(df)[colnames(df) == "logdistance"] <- "logdistance_t3"
colnames(df)[colnames(df)=="Pop_i"] <- "Pop_t"
colnames(df)[colnames(df)=="Pop_j"] <- "Pop_3"
UN2000_2 <- df

#targetcow = target country, ccode_3 = third country
UN_Sanct <- rbind(UN1900_2, UN2000_2)
UN_Sanct <- left_join(UN_Sanct, ICRG, by = c("year" = "Year", "ccode_3" = "ccode"))


##testing:##
#testdf <- select(UN_Sanct, year, targetcow, target_name, sender, ccode_3, thrd_ctry_name, bq)
#testdf <- testdf[testdf$year == 2000,]
#testdf <- testdf[testdf$targetcow == 700,]
#missings <- testdf[is.na(testdf$bq),]
#testicrg <- ICRG[ICRG$Year == 2000,]
#rm(testicrg,testdf,missings,testdf1)
##end testing##


UN_Sanct <- UN_Sanct[!is.na(UN_Sanct$ccode_3),]
UN_Sanct$index <- paste(
  UN_Sanct$year, UN_Sanct$sender, UN_Sanct$target_name_2, UN_Sanct$thrd_ctry_name,
  sep = "_")
UN_Sanct <- UN_Sanct[!is.na(UN_Sanct$ccode_3),]
UN_Sanct_PPI <- left_join(UN_Sanct, PPI, by = c("ccode_3" = "ccode"))
UN_Sanct <- UN_Sanct_PPI

##testing:##
#df1 <- UN_Sanct[UN_Sanct$sender == "UN",]
#df1 <- df1[df1$year == 2000,]
#rm(df1)
##end testing##


UN_Sanct_alliances <- left_join(UN_Sanct,
                                alliances_target_3rdctry,
                                by = c("year" = "year", "targetcow" = "target", "ccode_3" = "thrd_ctry"))
UN_Sanct_alliances$all_def_3t[is.na(UN_Sanct_alliances$all_def_3t)] <- 0
UN_Sanct_alliances$all_neut_3t[is.na(UN_Sanct_alliances$all_neut_3t)] <- 0
UN_Sanct_alliances$all_nonaggr_3t[is.na(UN_Sanct_alliances$all_nonaggr_3t)] <- 0
UN_Sanct_alliances$all_entente_3t[is.na(UN_Sanct_alliances$all_entente_3t)] <- 0

##testing:##
#testdf1 <- select(UN_Sanct_alliances, year, targetcow, target_name, sender, ccode_j, state_j, bq, starts_with("all_"))
#rm(testdf1)
##end testing##

UN_Sanct <- UN_Sanct_alliances
save(UN_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct2")
nrow(UN_Sanct)
rm(UN_Sanct, UN1900, UN1900_2, UN2000, UN2000_2, UN_Sanct_PPI, UN_Sanct_alliances)