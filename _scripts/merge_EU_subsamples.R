#For subsample analysis, EU data has to be transformed.

load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")

# this is limited to potential sanctions for computational reasons
df <- EU_Sanct[EU_Sanct$potential_sanction == 1,]
years <- unique(df$year)

c1994 <- c(200, 220, 230, 255, 260, 325)

for (year in years){
  obj <- paste0("df_", year)
  data <- df[df$year == year,]
  assign(obj, data)
}

for (year in years){
  orig <- paste0("df_", year)
  for (c in c1994){
    obj <- paste0("df_", year, "_c_", c)
    data <- get(orig)
    data$sender <- c
    assign(obj, data)
  }
}

rm(df_1989, df_1990, df_1991, df_1992, df_1993, df_1994, df_1995, df_1996, df_1997, df_1998, df_1999)
rm(df_2000, df_2001, df_2002, df_2003, df_2004, df_2005, df_2006, df_2007, df_2008, df_2009, df_2010)
rm(df_2011, df_2012, df_2013, df_2014)

df_list <- mget(ls(pattern = "*_c_*"))
gc(verbose = T)

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

rm(data, df, obj, orig, df1)

#df_list1900 <- mget(ls(pattern = "df_19*"))
#df_list2000 <- mget(ls(pattern = "df_20*"))

EU_Sanct_subsample <- bind_rows(df_list)
EU_Sanct <- EU_Sanct_subsample
save(EU_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct3")

rm(list = ls(pattern = "*_c_*"))
rm(list = ls(pattern = "df_19*"))
rm(list = ls(pattern = "df_2*"))
rm(EU_Sanct_subsample, EU_Sanct, df, df1, data, c, c1994, obj, orig, year, years)


