#analysis file:

library(plm)
library(jtools)
library(sjPlot)
library(sjmisc)
library(ggplot2)
#library(margins.plm)
library(interflex)
library(stargazer)
library(varhandle)
#define which analysis to run:

#If only potential cases, set p = T
p <- F

#Define the sample as string, either "full" or "US", later also "EU" and "UN"
s <- "full"

if (s == "full"){
  if (p == F){
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct2")
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct2")
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")
    Sanctions <- rbind(EU_Sanct, UN_Sanct, US_Sanct)
  } else if (p == T){
    #run potential sanction cases (can also be combinded with option "seperately")
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct_potential")
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct_potential")
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct_potential")
    Sanctions <- rbind(EU_Sanct, UN_Sanct, US_Sanct)
  }
} else if (s == "US"){
  if (p == F){
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct2")
    Sanctions <- US_Sanct
  } else if (p == T) {
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct_potential")
    Sanctions <- US_Sanct
  }
} else if (s == "EU"){
  if (p == F){
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct2")
    Sanctions <- EU_Sanct
  } else if (p == T){
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct_potential")
    Sanctions <- EU_Sanct
  }
} else if (s == "UN"){
  if (p == F){
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct2")
    Sanctions <- UN_Sanct
  } else if (p == T){
    load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/UN_Sanct_potential")
    Sanctions <- UN_Sanct
  }
}
rm(EU_Sanct, US_Sanct, UN_Sanct)
names(Sanctions)



#transform some trade flows:

#transform some trade flows:
Sanctions <- Sanctions[!is.na(Sanctions$Flow_3t),]
Sanctions$Flow_3t <- Sanctions$Flow_3t / (sqrt(var(Sanctions$Flow_3t))*2)
Sanctions$Flow_t3 <- Sanctions$Flow_t3 / (sqrt(var(Sanctions$Flow_t3))*2)
colnames(Sanctions)[colnames(Sanctions) == "sanction_dyad"] <- "sanct"
Sanctions$bq <- unfactor(Sanctions$bq)


lm1 <- lm(Flow_3t ~ sanct
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
          + factor(year) + factor(sender), data = Sanctions)

lm2 <- lm(Flow_3t ~ ability_trade
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
          + factor(year) + factor(sender), data = Sanctions)

lm3 <- lm(Flow_3t ~ sanct + ability_trade
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
          + factor(year) + factor(sender), data = Sanctions)

lm4_cap <- lm(Flow_3t ~  ability_trade * sanct
              + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
              + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
              + factor(year) + factor(sender) , data = Sanctions)

lm4_pol <- lm(Flow_3t ~ sanct * absidealimportantdiff_t3
              + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
              + Contig  + logdistance_t3 +all_entente_3t
              + factor(year) + factor(sender), data = Sanctions)


lm4_alliances <- lm(Flow_3t ~ sanct * all_entente_3t
                    + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                    + Contig  + logdistance_t3
                    + factor(year) + factor(sender), data = Sanctions)



cov1       <- vcovHC(lm1, type = "HC1", cluster = "time")
robust_se1   <- sqrt(diag(cov1))
cov2         <- vcovHC(lm2, type = "HC1", cluster = "time")
robust_se2  <- sqrt(diag(cov2))
cov3         <- vcovHC(lm3, type = "HC1", cluster = "time")
robust_se3   <- sqrt(diag(cov3))
cov4        <- vcovHC(lm4_cap, type = "HC1", cluster = "time")
robust_se4   <- sqrt(diag(cov4))
cov5        <- vcovHC(lm4_pol, type = "HC1", cluster = "time")
robust_se5   <- sqrt(diag(cov5))
cov6        <- vcovHC(lm4_alliances, type = "HC1", cluster = "time")
robust_se6   <- sqrt(diag(cov6))


stargazer(lm1, lm2, lm3, lm4_cap, lm4_pol, lm4_alliances,
          multicolumn = F,
          model.numbers= F,
          #add.lines = list(c("Year FE", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
          #                 (c("Sender FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))),
          single.row = F,
          initial.zero = F,
          omit.stat = c( "f", "aic", "bic","chi2","ser", "ll"),
          omit = c("year", "sender")
          ,se = list(robust_se1, robust_se2, robust_se3, robust_se4, robust_se5, robust_se6)
)


plot_model(lm4_cap, type = "pred", terms = c("ability_trade", "sanct"),
           title = "Predicted trade from third country to target, effect of PPI ability to control strategic trade and sanctions",
           axis.title = c("PPI ability to control strategic trade", "Normalized trade flows"),
           legend.title = "sanction",
           vcov.type = "HC1")