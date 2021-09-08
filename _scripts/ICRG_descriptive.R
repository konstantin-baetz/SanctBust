load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/ICRG/ICRG_full_new")

ICRG <- ICRG[ICRG$Year > 1988,]
mean(ICRG$bq, na.rm = T)
var(ICRG$bq, na.rm = T)

