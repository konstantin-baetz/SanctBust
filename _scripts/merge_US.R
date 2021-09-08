#US sample merging
#
# ATTENTION:
# do not run alone
# only in merge.R superscript
#############################


#US Sample
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct1")
US1900 <- US_Sanct[US_Sanct$year < "2000",]
US2000 <- US_Sanct[US_Sanct$year >= "2000",]
US1900_2 <- left_join(US1900, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))

df <- US1900_2[!is.na(US1900_2$year),]
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
US1900_2 <- df
rm(df)
US2000_2 <- left_join(US2000, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))
df <- US2000_2[!is.na(US2000_2$year),]
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
US2000_2 <- df
rm(df)


##testing:##
#testdf <- select(US1900_2, year, starts_with("targ"))
#all(testdf$targetcow == testdf$targetstate, na.rm = T)
#all(testdf$target_name == testdf$target_name_2, na.rm = T)
#testdf1 <- testdf[testdf$target_name != testdf$target_name_2,]
#rm(testdf)
##end testing##


#US to third country trade:
US1900_2 <- left_join(US1900_2, only_trade_US, by = c("year" = "year", "sender" = "sender", "ccode_3" = "ccode_j"))
US1900_2$ccode_i <- NULL
colnames(US1900_2)[colnames(US1900_2) == "Flow_i"] <- "Flow_s3"
colnames(US1900_2)[colnames(US1900_2) == "Flow_j"] <- "Flow_3s"
colnames(US1900_2)[colnames(US1900_2) == "logFlow_i"] <- "log_Flow_s3"
colnames(US1900_2)[colnames(US1900_2) == "logFlow_j"] <- "log_Flow_3s"
colnames(US1900_2)[colnames(US1900_2) == "logdistance"] <- "logdist_s3"
colnames(US1900_2)[colnames(US1900_2) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
colnames(US1900_2)[colnames(US1900_2) == "logGDP_i"] <- "logGDP_s"
colnames(US1900_2)[colnames(US1900_2) == "Pop_i"] <- "Pop_s"

US2000_2 <- left_join(US2000_2, only_trade_US, by = c("year" = "year", "sender" = "sender", "ccode_3" = "ccode_j"))
US2000_2$ccode_i <- NULL
colnames(US2000_2)[colnames(US2000_2) == "Flow_i"] <- "Flow_s3"
colnames(US2000_2)[colnames(US2000_2) == "Flow_j"] <- "Flow_3s"  
colnames(US2000_2)[colnames(US2000_2) == "logFlow_i"] <- "log_Flow_s3"
colnames(US2000_2)[colnames(US2000_2) == "logFlow_j"] <-  "log_Flow_3s"
colnames(US2000_2)[colnames(US2000_2) == "logdistance"] <- "logdist_s3"
colnames(US2000_2)[colnames(US2000_2) == "absidealimportantdiff"    ] <- "absidealimportantdiff_3s"
colnames(US2000_2)[colnames(US2000_2) == "logGDP_i"] <- "logGDP_s"
colnames(US2000_2)[colnames(US2000_2) == "Pop_i"] <- "Pop_s"

#US to target country trade:

US1900_2 <- left_join(US1900_2, only_trade_US, by = c("year" = "year", "sender" = "sender", "targetcow" = "ccode_j"))
US1900_2$ccode_i <- NULL
colnames(US1900_2)[colnames(US1900_2) == "Flow_i"] <- "Flow_st"
colnames(US1900_2)[colnames(US1900_2) == "Flow_j"] <- "Flow_ts"
colnames(US1900_2)[colnames(US1900_2) == "logFlow_i"] <- "log_Flow_st"
colnames(US1900_2)[colnames(US1900_2) == "logFlow_j"] <- "log_Flow_ts"
colnames(US1900_2)[colnames(US1900_2) == "logdistance"] <- "logdist_ts"
colnames(US1900_2)[colnames(US1900_2) == "absidealimportantdiff"    ] <- "absidealimportantdiff_ts"


US2000_2 <- left_join(US2000_2, only_trade_US, by = c("year" = "year", "sender" = "sender", "targetcow" = "ccode_j"))
US2000_2$ccode_i <- NULL
colnames(US2000_2)[colnames(US2000_2) == "Flow_i" ] <- "Flow_st"
colnames(US2000_2)[colnames(US2000_2) == "Flow_j"  ] <- "Flow_ts"  
colnames(US2000_2)[colnames(US2000_2) == "logFlow_i" ] <- "log_Flow_st"
colnames(US2000_2)[colnames(US2000_2) == "logFlow_j"   ] <-  "log_Flow_ts"
colnames(US2000_2)[colnames(US2000_2) == "logdistance"] <- "logdist_ts"
colnames(US2000_2)[colnames(US2000_2) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"



US_Sanct <- rbind(US1900_2, US2000_2)
#names(US_Sanct)
US_Sanct$logGDP_i <- NULL
US_Sanct$Pop_i <- NULL
US_Sanct$ccode <- NULL
US_Sanct$Contig <- NULL
names(US_Sanct)
US_Sanct <- left_join(US_Sanct, ICRG, by = c("year" = "Year", "ccode_3" = "ccode"))
US_Sanct <- US_Sanct[!is.na(US_Sanct$targetcow),]
US_Sanct <- US_Sanct[!is.na(US_Sanct$ccode_3),]
US_Sanct$index <- paste(US_Sanct$year, US_Sanct$sender, US_Sanct$ccode_t, US_Sanct$ccode_3, sep = "_")
US_Sanct_PPI <- left_join(US_Sanct, PPI, by = c("ccode_3" = "ccode"))
US_Sanct <- US_Sanct_PPI
US_Sanct <- US_Sanct[!is.na(US_Sanct$Flow_s3),]

#add alliance data

US_Sanct <- left_join(US_Sanct, alliances_US_to_3rdctry,
                      by = c("year" =  "year", "ccode_3" = "thrd_ctry"))

US_Sanct<- left_join(US_Sanct, alliances_target_3rdctry,
                     by = c("year" = "year", "targetcow" = "target", "ccode_3" = "thrd_ctry"))


#save US Sample
save(US_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct3")
nrow(US_Sanct)
rm(US_Sanct, US1900, US1900_2, US2000, US2000_2, US_Sanct_PPI)



#US Sample no additional trade data
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct1")
US1900 <- US_Sanct[US_Sanct$year < "2000",]
US2000 <- US_Sanct[US_Sanct$year >= "2000",]
US1900_2<- left_join(US1900, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))
df <- US1900_2[!is.na(US1900_2$year),]
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
US1900_2 <- df
rm(df)

US2000_2<- left_join(US2000, df_shrt, by = c("year" = "year", "targetcow" = "ccode_i"))
df <- US2000_2[!is.na(US2000_2$year),]
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
US2000_2 <- df
rm(df)


US_Sanct <- rbind(US1900_2, US2000_2)
US_Sanct <- left_join(US_Sanct, ICRG, by = c("year" = "Year", "ccode_3" = "ccode"))
US_Sanct <- US_Sanct[!is.na(US_Sanct$targetcow),]
US_Sanct$index <- paste(US_Sanct$year, US_Sanct$sender, US_Sanct$ccode, US_Sanct$ccode_j, sep = "_")
US_Sanct <- US_Sanct[!is.na(US_Sanct$targetcow),]
US_Sanct_PPI <- left_join(US_Sanct, PPI, by = c("ccode_3" = "ccode"))
US_Sanct <- US_Sanct_PPI


US_Sanct_alliances <- left_join(US_Sanct,
                                alliances_target_3rdctry,
                                by = c("year" = "year", "targetcow" = "target", "ccode_3" = "thrd_ctry"))
US_Sanct_alliances$all_def_3t[is.na(US_Sanct_alliances$all_def_3t)] <- 0
US_Sanct_alliances$all_neut_3t[is.na(US_Sanct_alliances$all_neut_3t)] <- 0
US_Sanct_alliances$all_nonaggr_3t[is.na(US_Sanct_alliances$all_nonaggr_3t)] <- 0
US_Sanct_alliances$all_entente_3t[is.na(US_Sanct_alliances$all_entente_3t)] <- 0
US_Sanct <- US_Sanct_alliances

save(US_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct2")
nrow(US_Sanct)
rm(US_Sanct, US1900, US1900_2, US2000, US2000_2, US_Sanct_PPI, US_Sanct_alliances, alliances_US_to_3rdctry, only_trade_US)

