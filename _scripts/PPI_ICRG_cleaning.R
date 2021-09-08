##cleaning of PPI file
library(readr)
library(readxl)
library(haven)
library(dplyr)
library(tidyr)


ccodes <- read_dta("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/ccodes/ccodes.dta")

ccodes <- unique(ccodes)
PPI_2019 <- read_delim("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/PPI/PPI_DISAGGREGATED.csv",
                                ";", escape_double = FALSE, trim_ws = TRUE)
PPI_2019  <- PPI_2019 [,2:8]
PPI_2019 $year <- 2019
PPI_2017 <- read_delim("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/PPI/PPI_2017.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
PPI_2017$rk <- NULL
PPI_2017$rk2 <- NULL
PPI_2017$year <- 2017

PPI <- rbind(PPI_2019 , PPI_2017)

PPI$country[PPI$country=="USA"] <- "United States of America"
PPI$country[PPI$country=="UK"] <- "United Kingdom"
PPI$country[PPI$country=="ROK"] <- "South Korea"
PPI$country[PPI$country=="DPRK"] <- "North Korea"
PPI$country[PPI$country=="Iran (Islamic Republic of)"] <- "Iran"
PPI$country[PPI$country=="Moldova (Rep of the)"] <- "Moldova"
PPI$country[PPI$country=="Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
PPI$country[PPI$country=="Tanzania (United Republic of)"] <- "Tanzania"
PPI$country[PPI$country=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
PPI$country[PPI$country=="Brunei Darussalam"] <- "Brunei"
PPI$country[PPI$country=="Lao People's Democratic Republic"] <- "Laos"
PPI$country[PPI$country=="Viet Nam"] <- "Vietnam"
PPI$country[PPI$country=="Syrian Arab Republic"] <- "Syria"
PPI$country[PPI$country=="Solomon;Islands"] <- "Solomon Islands"
PPI$country[PPI$country=="Trinidad & Tobago"] <- "Trinidad and Tobago"
PPI$country[PPI$country=="Timor-Leste"] <- "East Timor"
PPI$country[PPI$country=="Cote d'Ivoire"] <- "Ivory Coast"
PPI$country[PPI$country=="Saint Vincent & the Grenadines"] <- "St. Vincent and the Grenadines"
PPI$country[PPI$country=="Holy See"] <- "Papal States"
PPI$country[PPI$country=="Sao Tome & Principe"] <- "Sao Tome and Principe"
PPI$country[PPI$country=="Congo (Dem Rep of the)"] <- "Democratic Republic of the Congo"
PPI$country[PPI$country=="Congo (Rep of the)"] <- "Congo"
PPI$country[PPI$country=="Saint Kitts & Nevis"] <- "Saint Kitts and Nevis"
PPI$country[PPI$country=="Serbia"] <- "Yugoslavia"


PPI <- left_join(PPI, ccodes, by = c("country" = "statenme"))

rm(PPI_2017, PPI_2019)

save(PPI, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/PPI/PPI_full_new")

rm(PPI)

ICRG <- read_excel("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/ICRG/ICRG_bureaucraticquality.xlsx",
                                       sheet = "Sheet2")
ICRG$Country[ICRG$Country=="Trinidad & Tobago"] <- "Trinidad and Tobago"
ICRG$Country[ICRG$Country=="UAE"] <- "United Arab Emirates"
ICRG$Country[ICRG$Country=="Cote d'Ivoire"] <- "Ivory Coast"
ICRG$Country[ICRG$Country=="Congo, DR"] <- "Democratic Republic of the Congo"
ICRG$Country[ICRG$Country=="East Germany"] <- "German Democratic Republic"
ICRG$Country[ICRG$Country=="West Germany"] <- "German Federal Republic"
ICRG$Country[ICRG$Country=="United States"] <- "United States of America"
ICRG$Country[ICRG$Country=="Korea, DPR"] <- "North Korea"


ICRG <- left_join(ICRG, ccodes, by=c("Country"="statenme"))
ICRG <- as.data.frame(as.matrix(ICRG))
ICRG$ccode <- as.numeric(as.character(ICRG$ccode))
ICRG$Year <- as.numeric(as.character(ICRG$Year))
ICRG <- ICRG[!is.na(ICRG$bq),]
save(ICRG, file = "C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/ICRG/ICRG_full_new")
#state_capacity <- left_join(ICRG, PPI, by = c("ccode" = "ccode", "Year" = "year"))
rm(ICRG, ccodes)
