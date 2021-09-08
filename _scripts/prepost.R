library(dplyr)
#library(rdd)


#To generate pre post comparison, I merge pre-post trade flows to sanction cases

load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct3")
#pre_post_dummies <- EUSANCT
#rm(EUSANCT)


#trade data:
#load("C:/Users/Konst/Google Drive/Studium/Dissertation/paper1/_data/df.RData")
#df1$year <- as.numeric(as.character(df1$year))
#df <- df1[df1$year > 1988,]
#rm(df1)
#df_shrt <- select(df, year, Flow_i, Flow_j, ccode_i, ccode_j, state_i, state_j, logGDP_i, logGDP_j, Pop_i, Pop_j, Contig, Region_i, Region_j, absidealimportantdiff, logdistance)
#df_shrt$year <- as.numeric(df_shrt$year)
#df_shrt$logFlow_j <- log(df_shrt$Flow_j+1)
#df_shrt <- df_shrt[df_shrt$ccode_i == 2,]
#rm(df)


#this generates a datesets of US sanction dyads
#with pre post sanction dummies as well as trade flows:
#df <- left_join(US_Sanct, df_shrt, by = c("year" = "year", "ccodecow" = "ccode_j"))


#drop all obs that are not related to a sanction
df <- US_Sanct
sanct_only <- df[df$pre_sanct_any5 == 1 | df$post_sanct_any5 == 1,]

first_sanct <- sanct_only[sanct_only$first_sanct == 1,]
pre_sanct_1 <- sanct_only[sanct_only$pre_sanct_1 == 1,]
pre_sanct_2 <- sanct_only[sanct_only$pre_sanct_2 == 1,]
pre_sanct_3 <- sanct_only[sanct_only$pre_sanct_3 == 1,]
pre_sanct_4 <- sanct_only[sanct_only$pre_sanct_4 == 1,]
pre_sanct_5 <- sanct_only[sanct_only$pre_sanct_5 == 1,]

last_sanct <- sanct_only[sanct_only$last_sanct == 1,]
post_sanct_1 <- sanct_only[sanct_only$post_sanct_1 == 1,]
post_sanct_2 <- sanct_only[sanct_only$post_sanct_2 == 1,]
post_sanct_3 <- sanct_only[sanct_only$post_sanct_3 == 1,]
post_sanct_4 <- sanct_only[sanct_only$post_sanct_4 == 1,]
post_sanct_5 <- sanct_only[sanct_only$post_sanct_5 == 1,]



mean(pre_sanct_5$log_Flow_st, na.rm = T, trim = 0.05)
sd(pre_sanct_5$log_Flow_st, na.rm = T)
mean(pre_sanct_4$log_Flow_st, na.rm = T, trim = 0.05)
sd(pre_sanct_4$log_Flow_st, na.rm = T)
mean(pre_sanct_3$log_Flow_st, na.rm = T, trim = 0.05)
mean(pre_sanct_2$log_Flow_st, na.rm = T, trim = 0.05)
mean(pre_sanct_1$log_Flow_st, na.rm = T, trim = 0.05)
mean(first_sanct$log_Flow_st, na.rm = T, trim = 0.05)

mean(last_sanct$log_Flow_st, na.rm =  T, trim = 0.05)
mean(post_sanct_1$log_Flow_st, na.rm = T, trim = 0.05)
mean(post_sanct_2$log_Flow_st, na.rm = T, trim = 0.05)
mean(post_sanct_3$log_Flow_st, na.rm = T, trim = 0.05)
mean(post_sanct_4$log_Flow_st, na.rm = T, trim = 0.05)
mean(post_sanct_5$log_Flow_st, na.rm = T, trim = 0.05)






sanct_only_low <- sanct_only[sanct_only$bq<=1,]
sanct_only_mid <- sanct_only[sanct_only$bq>1 & sanct_only$bq <3,]
sanct_only_high <- sanct_only[sanct_only$bq>=3,]


first_sanct_low <- sanct_only_low[sanct_only_low$first_sanct == 1,]
pre_sanct_1_low <- sanct_only_low[sanct_only_low$pre_sanct_1 == 1,]
pre_sanct_2_low <- sanct_only_low[sanct_only_low$pre_sanct_2 == 1,]
pre_sanct_3_low <- sanct_only_low[sanct_only_low$pre_sanct_3 == 1,]
pre_sanct_4_low <- sanct_only_low[sanct_only_low$pre_sanct_4 == 1,]
pre_sanct_5_low <- sanct_only_low[sanct_only_low$pre_sanct_5 == 1,]

last_sanct_low <- sanct_only_low[sanct_only_low$last_sanct == 1,]
post_sanct_1_low <- sanct_only_low[sanct_only_low$post_sanct_1 == 1,]
post_sanct_2_low <- sanct_only_low[sanct_only_low$post_sanct_2 == 1,]
post_sanct_3_low <- sanct_only_low[sanct_only_low$post_sanct_3 == 1,]
post_sanct_4_low <- sanct_only_low[sanct_only_low$post_sanct_4 == 1,]
post_sanct_5_low <- sanct_only_low[sanct_only_low$post_sanct_5 == 1,]

mean(pre_sanct_5_low$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_4_low$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_3_low$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_2_low$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_1_low$Flow_3t, na.rm = T, trim = 0.05)
mean(first_sanct_low$Flow_3t, na.rm = T, trim = 0.05)

mean(last_sanct_low$Flow_3t, na.rm =  T, trim = 0.05)
mean(post_sanct_1_low$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_2_low$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_3_low$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_4_low$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_5_low$Flow_3t, na.rm = T, trim = 0.05)



first_sanct_mid <- sanct_only_mid[sanct_only_mid$first_sanct == 1,]
pre_sanct_1_mid <- sanct_only_mid[sanct_only_mid$pre_sanct_1 == 1,]
pre_sanct_2_mid <- sanct_only_mid[sanct_only_mid$pre_sanct_2 == 1,]
pre_sanct_3_mid <- sanct_only_mid[sanct_only_mid$pre_sanct_3 == 1,]
pre_sanct_4_mid <- sanct_only_mid[sanct_only_mid$pre_sanct_4 == 1,]
pre_sanct_5_mid <- sanct_only_mid[sanct_only_mid$pre_sanct_5 == 1,]

last_sanct_mid <- sanct_only_mid[sanct_only_mid$last_sanct == 1,]
post_sanct_1_mid <- sanct_only_mid[sanct_only_mid$post_sanct_1 == 1,]
post_sanct_2_mid <- sanct_only_mid[sanct_only_mid$post_sanct_2 == 1,]
post_sanct_3_mid <- sanct_only_mid[sanct_only_mid$post_sanct_3 == 1,]
post_sanct_4_mid <- sanct_only_mid[sanct_only_mid$post_sanct_4 == 1,]
post_sanct_5_mid <- sanct_only_mid[sanct_only_mid$post_sanct_5 == 1,]
mean(pre_sanct_5_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_4_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_3_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_2_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_1_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(first_sanct_mid$Flow_3t, na.rm = T, trim = 0.05)

mean(last_sanct_mid$Flow_3t, na.rm =  T, trim = 0.05)
mean(post_sanct_1_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_2_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_3_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_4_mid$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_5_mid$Flow_3t, na.rm = T, trim = 0.05)




first_sanct_high <- sanct_only_high[sanct_only_high$first_sanct == 1,]
pre_sanct_1_high <- sanct_only_high[sanct_only_high$pre_sanct_1 == 1,]
pre_sanct_2_high <- sanct_only_high[sanct_only_high$pre_sanct_2 == 1,]
pre_sanct_3_high <- sanct_only_high[sanct_only_high$pre_sanct_3 == 1,]
pre_sanct_4_high <- sanct_only_high[sanct_only_high$pre_sanct_4 == 1,]
pre_sanct_5_high <- sanct_only_high[sanct_only_high$pre_sanct_5 == 1,]
last_sanct_high <- sanct_only_high[sanct_only_high$last_sanct == 1,]
post_sanct_1_high <- sanct_only_high[sanct_only_high$post_sanct_1 == 1,]
post_sanct_2_high <- sanct_only_high[sanct_only_high$post_sanct_2 == 1,]
post_sanct_3_high <- sanct_only_high[sanct_only_high$post_sanct_3 == 1,]
post_sanct_4_high <- sanct_only_high[sanct_only_high$post_sanct_4 == 1,]
post_sanct_5_high <- sanct_only_high[sanct_only_high$post_sanct_5 == 1,]


mean(pre_sanct_5_high$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_4_high$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_3_high$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_2_high$Flow_3t, na.rm = T, trim = 0.05)
mean(pre_sanct_1_high$Flow_3t, na.rm = T, trim = 0.05)
mean(first_sanct_high$Flow_3t, na.rm = T, trim = 0.05)

mean(last_sanct_high$Flow_3t, na.rm =  T, trim = 0.05)
mean(post_sanct_1_high$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_2_high$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_3_high$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_4_high$Flow_3t, na.rm = T, trim = 0.05)
mean(post_sanct_5_high$Flow_3t, na.rm = T, trim = 0.05)



## every observation should 

