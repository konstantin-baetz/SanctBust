
#For subsample analysis, EU data has to be transformed.

load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")

# this is limited to potential sanctions for computational reasons
df <- EU_Sanct[EU_Sanct$potential_sanction == 1,]
years <- unique(df$year)

c1994 <- c(211, 220, 255, 260, 325, 212, 210, 390, 205, 200, 350, 235, 230)
c2003 <- c(305, 375, 380, c1994)
c2006 <- c(352, 315, 316, 366, 310, 367, 368, 338, 290, 317, 349, c2003)
c2012 <- c(355, 360, c2006)
c2016 <- c(344, c2012)

y1994 <- years[years <= 1994]
y2003 <- years[years > 1994 & years <= 2003]
y2006 <- years[years > 2003 & years <= 2006]
y2012 <- years[years > 2006 & years <= 2012]
y2016 <- years[years > 2012 & years <= 2016]


for (year in years){
  obj <- paste0("df_", year)
  data <- df[df$year == year,]
  assign(obj, data)
}

for (year in y1994){
  orig <- paste0("df_", year)
  for (c in c1994){
    obj <- paste0("df_", year, "_c_", c)
    data <- get(orig)
    data$sender <- c
    assign(obj, data)
  }
}

for (year in y2003){
  orig <- paste0("df_", year)
  for (c in c2003){
    obj <- paste0("df_", year, "_c_", c)
    data <- get(orig)
    data$sender <- c
    assign(obj, data)
  }
}


for (year in y2006){
  orig <- paste0("df_", year)
  for (c in c2006){
    obj <- paste0("df_", year, "_c_", c)
    data <- get(orig)
    data$sender <- c
    assign(obj, data)
  }
}

for (year in y2012){
  orig <- paste0("df_", year)
  for (c in c2012){
    obj <- paste0("df_", year, "_c_", c)
    data <- get(orig)
    data$sender <- c
    assign(obj, data)
  }
}

for (year in y2016){
  orig <- paste0("df_", year)
  for (c in c2016){
    obj <- paste0("df_", year, "_c_", c)
    data <- get(orig)
    data$sender <- c
    assign(obj, data)
  }
}


df_list89 <- mget(ls(pattern = "*89_c_*"))
df_list90 <- mget(ls(pattern = "*90_c_*"))
df_list91 <- mget(ls(pattern = "*91_c_*"))
df_list92 <- mget(ls(pattern = "*92_c_*"))
df_list93 <- mget(ls(pattern = "*93_c_*"))
df_list94 <- mget(ls(pattern = "*94_c_*"))
df_list95 <- mget(ls(pattern = "*95_c_*"))
df_list96 <- mget(ls(pattern = "*96_c_*"))
df_list97 <- mget(ls(pattern = "*97_c_*"))
df_list98 <- mget(ls(pattern = "*98_c_*"))
df_list99 <- mget(ls(pattern = "*99_c_*"))
df_list00 <- mget(ls(pattern = "*00_c_*"))
df_list01 <- mget(ls(pattern = "*01_c_*"))
df_list02 <- mget(ls(pattern = "*02_c_*"))
df_list03 <- mget(ls(pattern = "*03_c_*"))
df_list04 <- mget(ls(pattern = "*04_c_*"))
df_list05 <- mget(ls(pattern = "*05_c_*"))
df_list06 <- mget(ls(pattern = "*06_c_*"))
df_list07 <- mget(ls(pattern = "*07_c_*"))
df_list08 <- mget(ls(pattern = "*08_c_*"))
df_list09 <- mget(ls(pattern = "*09_c_*"))
df_list10 <- mget(ls(pattern = "*10_c_*"))
df_list11 <- mget(ls(pattern = "*11_c_*"))
df_list12 <- mget(ls(pattern = "*12_c_*"))
df_list13 <- mget(ls(pattern = "*13_c_*"))
df_list14 <- mget(ls(pattern = "*14_c_*"))
df_list15 <- mget(ls(pattern = "*15_c_*"))
gc()




df_list <- df_list89
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1989 <- bind_rows(df_list)
EU_Sanct_subsamples <- EU_Sanct_subsample1989
rm(EU_Sanct_subsample1989)
rm(df_list89)
rm(list = ls(pattern = "df_1989*"))

df_list <- df_list90
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1990 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1990)
rm(EU_Sanct_subsample1990)
rm(df_list90)
rm(list = ls(pattern = "df_1990*"))

df_list <- df_list91
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1991 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1991)
rm(EU_Sanct_subsample1991)
rm(df_list91)
rm(list = ls(pattern = "df_1991*"))

gc(verbose=T)
gc(verbose=T)

df_list <- df_list92
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1992 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1992)
rm(EU_Sanct_subsample1992)
rm(df_list92)
rm(list = ls(pattern = "df_1992*"))

df_list <- df_list93
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1993 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1993)
rm(EU_Sanct_subsample1993)
rm(df_list93)
rm(list = ls(pattern = "df_1993*"))





df_list <- df_list94
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1994 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1994)
rm(EU_Sanct_subsample1994)
rm(df_list94)
rm(list = ls(pattern = "df_1994*"))
gc()

df_list <- df_list95
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1995 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1995)
rm(EU_Sanct_subsample1995)
rm(df_list95)
rm(list = ls(pattern = "df_1995*"))
gc(verbose=T)


df_list <- df_list96
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1996 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1996)
rm(EU_Sanct_subsample1996)
rm(df_list96)
rm(list = ls(pattern = "df_1996*"))

df_list <- df_list97
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1997 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1997)
rm(EU_Sanct_subsample1997)
rm(df_list97)
rm(list = ls(pattern = "df_1997*"))

df_list <- df_list98
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1998 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1998)
rm(EU_Sanct_subsample1998)
rm(df_list98)
rm(list = ls(pattern = "df_1998*"))

df_list <- df_list99
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample1999 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample1999)
rm(EU_Sanct_subsample1999)
rm(df_list99)
rm(list = ls(pattern = "df_1999*"))

df_list <- df_list00
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2000 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2000)
rm(EU_Sanct_subsample2000)
rm(df_list00)
rm(list = ls(pattern = "df_2000*"))

df_list <- df_list01
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2001 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2001)
rm(EU_Sanct_subsample2001)
rm(df_list01)
rm(list = ls(pattern = "df_2001*"))

df_list <- df_list02
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2002 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2002)
rm(EU_Sanct_subsample2002)
rm(df_list02)
rm(list = ls(pattern = "df_2002*"))

df_list <- df_list03
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2003 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2003)
rm(EU_Sanct_subsample2003)
rm(df_list03)
rm(list = ls(pattern = "df_2003*"))

df_list <- df_list04
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2004 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2004)
rm(EU_Sanct_subsample2004)
rm(df_list04)
rm(list = ls(pattern = "df_2004*"))

gc(verbose=T)
gc(verbose=T)

df_list <- df_list05
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2005 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2005)
rm(EU_Sanct_subsample2005)
rm(df_list05)
rm(list = ls(pattern = "df_2005*"))

df_list <- df_list06
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2006 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2006)
rm(EU_Sanct_subsample2006)
rm(df_list06)
rm(list = ls(pattern = "df_2006*"))

df_list <- df_list07
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2007 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2007)
rm(EU_Sanct_subsample2007)
rm(df_list07)
rm(list = ls(pattern = "df_2007*"))

df_list <- df_list08
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2008 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2008)
rm(EU_Sanct_subsample2008)
rm(df_list08)
rm(list = ls(pattern = "df_2008*"))

df_list <- df_list09
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2009 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2009)
rm(EU_Sanct_subsample2009)
rm(df_list09)
rm(list = ls(pattern = "df_2009*"))
gc(verbose = T)
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct_subsample_temp2009")


df_list <- df_list10
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2010 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2010)
rm(EU_Sanct_subsample2010)
rm(df_list10)
rm(list = ls(pattern = "df_2010*"))

df_list <- df_list11
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2011 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2011)
rm(EU_Sanct_subsample2011)
rm(df_list11)
rm(list = ls(pattern = "df_2011*"))

df_list <- df_list12
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2012 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2012)
rm(EU_Sanct_subsample2012)
rm(df_list12)
rm(list = ls(pattern = "df_2012*"))

gc()

df_list <- df_list13
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2013 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2013)
rm(EU_Sanct_subsample2013)
rm(df_list13)
rm(list = ls(pattern = "df_2013*"))

gc()

df_list <- df_list14
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2014 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2014)
rm(EU_Sanct_subsample2014)
rm(df_list14)
rm(list = ls(pattern = "df_2014*"))

gc()

df_list <- df_list15
for (df1 in df_list){
  df <- left_join(df1, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "ccode_3" = "ccode_j"))
  df$ccode_i <- NULL
  df$Contig.x <- NULL
  df$Contig.y <- NULL
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_s3"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3s"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_s3"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_3s"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_s3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_3s"
  colnames(df)[colnames(df) == "logGDP_i"] <- "logGDP_s"
  colnames(df)[colnames(df) == "Pop_i"] <- "Pop_s"
  df <- left_join(df, df_shrt, by = c("year" = "year", "sender" = "ccode_i", "targetcow" = "ccode_j"))
  colnames(df)[colnames(df) == "Flow_i"] <- "Flow_st"
  colnames(df)[colnames(df) == "Flow_j"] <- "Flow_3t"
  colnames(df)[colnames(df) == "logFlow_i"] <- "log_Flow_st"
  colnames(df)[colnames(df) == "logFlow_j"] <- "log_Flow_ts"
  colnames(df)[colnames(df) == "logdistance"] <- "logdist_t3"
  colnames(df)[colnames(df) == "absidealimportantdiff"] <- "absidealimportantdiff_ts"
  df$state_i.x <- NULL
  df$state_j.x <- NULL
  df$Pop_j.x <- NULL
  df$state_i.y <- NULL
  df$state_j.y <- NULL
  df$Pop_j.y <- NULL
  df$logGDP_j.x <- NULL
  df$logGDP_j.y <- NULL
  df <- df[!is.na(df$targetcow),]
  df <- df[!is.na(df$ccode_3),]
  #to save
  year <- df$year[1]
  c <- df$sender[1]
  obj <- paste0("df_", year, "_c_", c)
  assign(obj, df)
  gc()
  print(paste0("successfully merged ", year, " for country code ", c))
}
EU_Sanct_subsample2015 <- bind_rows(df_list)
EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subsample2015)
rm(EU_Sanct_subsample2015)
rm(df_list15)
rm(list = ls(pattern = "df_2015*"))
gc(verbose=T)
EU_Sanct_subspost2009 <- EU_Sanct_subsamples
save(EU_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct3")
save(EU_Sanct_subspost2009, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_subsamples_post2009")
rm(only_trade, only_trade_US, EU_Sanct, alliances_target_3rdctry,
   alliances_US_to_3rdctry, year, years, df1, obj, orig)
rm(list = ls(pattern = "y1*"))
rm(list = ls(pattern = "y2*"))
#rm(list = ls(pattern = "c*"))


load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct_subsample_temp2009")
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_subsamples_post2009")

EU_Sanct_subsamples <- rbind(EU_Sanct_subsamples, EU_Sanct_subspost2009)