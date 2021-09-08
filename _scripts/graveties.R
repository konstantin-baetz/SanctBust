#gravity models:
library(gravity)

load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct2")
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct2")
load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")
Sanctions <- rbind(EU_Sanct, UN_Sanct, US_Sanct)
