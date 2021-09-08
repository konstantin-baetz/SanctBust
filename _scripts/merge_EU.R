#EU sample merging
#
# ATTENTION:
# do not run alone
# only in merge.R superscript
#############################

load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct1")
EU1900 <- EU_Sanct[EU_Sanct$year < "2000",]
EU2000 <- EU_Sanct[EU_Sanct$year >= "2000",]
EU1900_2<- left_join(EU1900, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))
df <- EU1900_2[!is.na(EU1900_2$year),]
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
EU1900_2 <- df


EU2000_2<- left_join(EU2000, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))
df <- EU2000_2[!is.na(EU2000_2$year),]
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
EU2000_2 <- df

#targetcow = target country, ccode_3 = third country
EU_Sanct <- rbind(EU1900_2, EU2000_2)
rm(EU1900, EU1900_2, EU2000, EU2000_2)
EU_Sanct <- left_join(EU_Sanct, ICRG, by = c("year" = "Year", "ccode_3" = "ccode"))


##testing:##
#testdf <- select(EU_Sanct, year, targetcow, target_name, sender, ccode_3, thrd_ctry_name, bq)
#testdf <- testdf[testdf$year == 2000,]
#testdf <- testdf[testdf$targetcow == 700,]
#missings <- testdf[is.na(testdf$bq),]
#testicrg <- ICRG[ICRG$Year == 2000,]
#rm(testicrg,testdf,missings,testdf1)
##end testing##


EU_Sanct <- EU_Sanct[!is.na(EU_Sanct$ccode_3),]
EU_Sanct$index <- paste(
  EU_Sanct$year, EU_Sanct$sender, EU_Sanct$target_name_2, EU_Sanct$thrd_ctry_name,
  sep = "_")
EU_Sanct <- EU_Sanct[!is.na(EU_Sanct$ccode_3),]
EU_Sanct_PPI <- left_join(EU_Sanct, PPI, by = c("ccode_3" = "ccode"))
EU_Sanct <- EU_Sanct_PPI

##testing:##
#df1 <- EU_Sanct[EU_Sanct$sender == "UN",]
#df1 <- df1[df1$year == 2000,]
#rm(df1)
##end testing##


EU_Sanct_alliances <- left_join(EU_Sanct,
                                alliances_target_3rdctry,
                                by = c("year" = "year", "targetcow" = "target", "ccode_3" = "thrd_ctry"))
EU_Sanct_alliances$all_def_3t[is.na(EU_Sanct_alliances$all_def_3t)] <- 0
EU_Sanct_alliances$all_neut_3t[is.na(EU_Sanct_alliances$all_neut_3t)] <- 0
EU_Sanct_alliances$all_nonaggr_3t[is.na(EU_Sanct_alliances$all_nonaggr_3t)] <- 0
EU_Sanct_alliances$all_entente_3t[is.na(EU_Sanct_alliances$all_entente_3t)] <- 0

##testing:##
#testdf1 <- select(EU_Sanct_alliances, year, targetcow, target_name, sender, ccode_j, state_j, bq, starts_with("all_"))
#rm(testdf1)
##end testing##

EU_Sanct <- EU_Sanct_alliances
save(EU_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")
nrow(EU_Sanct)
rm(EU_Sanct, EU_Sanct_PPI, EU_Sanct_alliances)
#years <- unique(EU_Sanct$year)
#years








