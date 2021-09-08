library(readxl)
library(mefa)
library(janitor)
library(dplyr)
library(haven)



#this script generates three datasets, one per sender (EU, US, UN),
#with every possible third country as well as that countries 
#bq index and trade volumnes.

#datasets are saved as /_data/EUSANCT/XX_Sanct2 with XX = {EU, US, UN}

#trade data:
load("C:/Users/Konst/Google Drive/Studium/Dissertation/paper1/_data/df.RData")
df1$year <- as.numeric(as.character(df1$year))
df1$ccode_i <- as.numeric(df1$ccode_i)
df1$ccode_j <- as.numeric(df1$ccode_j)

df <- df1[df1$year > 1988,]
rm(df1)
df_shrt <- select(df, year, Flow_i, Flow_j, ccode_i, ccode_j,
                  state_i, state_j, logGDP_i, logGDP_j, Pop_i, Pop_j,
                  Contig, absidealimportantdiff, logdistance)
df_shrt$year <- as.numeric(df_shrt$year)
df_shrt$logFlow_j <- log(df_shrt$Flow_j+1)
df_shrt$logFlow_i <- log(df_shrt$Flow_i+1)
rm(df)




#load the stata capacity indicators:
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/ICRG/ICRG_full_new")
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/PPI/PPI_full_new")
PPI <- PPI[PPI$year == 2017,]
PPI <- select(PPI, points, intl_commitment, legislation, ability_trade,
              ability_financing, enforcement, ccode)

#load the alliance data:
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/COW/alliances_US_to_3rdctry")
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/COW/alliances_target_3rdctry")

#EU Sample
source("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_scripts/merge_EU.R")
#source("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_scripts/merge_EU_subsamples.R")
#UN Sample
source("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_scripts/merge_UN.R")

only_trade <- select(df_shrt, year, Flow_i, Flow_j, ccode_i, ccode_j,
                     logFlow_i, logFlow_j, logdistance, logGDP_i,
                     absidealimportantdiff, Pop_i)
only_trade_US <- only_trade[only_trade$ccode_i == 2,]
only_trade_US$sender <- "US"


#US Sample
source("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_scripts/merge_US.R")


#end of script
rm(df, df_shrt, ICRG, PPI, alliances_target_3rdctry, alliances_US_to_3rdctry,
   only_trade, only_trade_US)


# for analysis of only likely sanction cases:
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct2")
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct2")
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")
EU_Sanct <- EU_Sanct[EU_Sanct$potential_sanction == 1,]
UN_Sanct <- UN_Sanct[UN_Sanct$potential_sanction == 1,]
US_Sanct <- US_Sanct[US_Sanct$potential_sanction == 1,]
save(EU_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct_potential")
save(UN_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct_potential")
save(US_Sanct, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct_potential")
rm(EU_Sanct, US_Sanct, UN_Sanct)