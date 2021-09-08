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
s <- "UN"

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
Sanctions <- Sanctions[!is.na(Sanctions$Flow_3t),]
Sanctions$Flow_3t <- Sanctions$Flow_3t / (sqrt(var(Sanctions$Flow_3t))*2)
Sanctions$Flow_t3 <- Sanctions$Flow_t3 / (sqrt(var(Sanctions$Flow_t3))*2)
colnames(Sanctions)[colnames(Sanctions) == "sanction_dyad"] <- "sanct"
Sanctions$bq <- unfactor(Sanctions$bq)


lm1 <- lm(Flow_3t ~ sanct
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
            + factor(year) + factor(sender), data = Sanctions)

lm2 <- lm(Flow_3t ~ bq
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
          + factor(year) + factor(sender), data = Sanctions)

lm3 <- lm(Flow_3t ~ sanct + bq
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
           + factor(year) + factor(sender), data = Sanctions)

lm4_cap <- lm(Flow_3t ~  bq * sanct
              + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
              + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
         + factor(year) , data = Sanctions)



lm4_pol <- lm(Flow_3t ~ sanct * absidealimportantdiff_t3  + bq
              + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
              + Contig  + logdistance_t3 +all_entente_3t
              + factor(year) , data = Sanctions)


lm4_alliances <- lm(Flow_3t ~ sanct * all_entente_3t  + bq
              + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
              + Contig  + absidealimportantdiff_t3 + logdistance_t3
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


summ(lm4_cap, cluster = c("year", "targetcow"))
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4_cap)
summary(lm5_cap)
summary(lm6_cap)
summary(lm4_pol)
summary(lm5_pol)
summary(lm6_pol)
summary(lm4_alliances)

plot_model(lm4_cap, type = "pred", terms = c("bq", "sanct"),
           title = "Predicted trade from third country to target, effect of bureaucratic quality and sanctions (UN Sample)",
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Sanction",
           vcov.type = "HC1")

plot_model(lm4_pol, type = "pred", terms = c("absidealimportantdiff_t3" , "sanct"),
           title = "Predicted trade from third country to target, political alignment and sanctions (UN Sample)",
           axis.title = c("Political alignment (UNGA votes)", "Normalized trade flows"),
           legend.title = "Sanction",
           vcov.type = "HC1")





#plot_model(lm4_cap, type = "pred", terms = c("sanct", "bq"))
plot_model(lm5_cap, type = "pred", terms = c("bq", "pre_sanct_any5"),
           title = "Predicted trade from third country to target, effect of Bureaucratic quality and placebo treatment",
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Five years before sanction",
           vcov.type = "HC1")
plot_model(lm6_cap, type = "pred", terms = c("bq", "post_sanct_any5"),
           title = "Predicted trade from third country to target, effect of Bureaucratic quality and placebo treatment",
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Five years after sanction",
           vcov.type = "HC1")
plot_model(lm4_pol, type = "pred", terms = c("absidealimportantdiff_t3", "sanct"))
plot_model(lm5_pol, type = "pred", terms = c("absidealimportantdiff_t3", "pre_sanct_any5"))
plot_model(lm6_pol, type = "pred", terms = c("absidealimportantdiff_t3", "post_sanct_any5"))


#placebo treatments:

placebo_1 <- lm(Flow_3t ~ pre_sanct_5 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
              + factor(year), data = Sanctions)
placebo_coeff_1 <- placebo_1$coefficients[37]
summary(placebo_1)


placebo_2 <- lm(Flow_3t ~ pre_sanct_4 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_2 <- placebo_2$coefficients[37]

placebo_3 <- lm(Flow_3t ~ pre_sanct_3 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_3 <- placebo_3$coefficients[37]

placebo_4 <- lm(Flow_3t ~ pre_sanct_2 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_4 <- placebo_4$coefficients[37]

placebo_5 <- lm(Flow_3t ~ pre_sanct_1 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_5 <- placebo_5$coefficients[37]

placebo_6 <- lm(Flow_3t ~ sanct * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_6 <- placebo_6$coefficients[37]

placebo_7 <- lm(Flow_3t ~ post_sanct_1 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_7 <- placebo_7$coefficients[37]

placebo_8 <- lm(Flow_3t ~ post_sanct_2 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_8 <- placebo_8$coefficients[37]

placebo_9 <- lm(Flow_3t ~ post_sanct_3 * bq 
                + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_9 <- placebo_9$coefficients[37]

placebo_10 <- lm(Flow_3t ~ post_sanct_4 * bq 
                 + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                 + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_10 <- placebo_10$coefficients[37]

placebo_11 <- lm(Flow_3t ~ post_sanct_5 * bq 
                 + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                 + Contig + absidealimportantdiff_t3 + logdistance_t3
                + factor(year), data = Sanctions)
placebo_coeff_11 <- placebo_11$coefficients[37]

coefs <- c(placebo_coeff_1, placebo_coeff_2, placebo_coeff_3, placebo_coeff_4,
           placebo_coeff_5, placebo_coeff_6, placebo_coeff_7, placebo_coeff_8,
           placebo_coeff_9, placebo_coeff_10, placebo_coeff_11)

t_array <- c( -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)

placebo_test <- cbind(t_array, coefs)

plot_model(placebo_1, type = "pred", terms = c("bq", "pre_sanct_5"),
           title = "Predicted trade from third country to target, effect of Bureaucratic quality and sanctions",
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Placebo Sanction",
           vcov.type = "HC1")



#all alliances in comparison:
lm4_def <- lm(Flow_3t ~ sanct * all_def_3t
                    + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                    + Contig  + logdistance_t3
                    + factor(year) + factor(sender), data = Sanctions)
lm4_neut <- lm(Flow_3t ~ sanct * all_neut_3t
                    + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                    + Contig  + logdistance_t3
                    + factor(year) + factor(sender), data = Sanctions)
lm4_nonaggr <- lm(Flow_3t ~ sanct * all_nonaggr_3t
                    + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                    + Contig  + logdistance_t3
                    + factor(year) + factor(sender), data = Sanctions)
lm4_entente <- lm(Flow_3t ~ sanct * all_entente_3t
                    + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
                    + Contig  + logdistance_t3
                    + factor(year) + factor(sender), data = Sanctions)


cov_def       <- vcovHC(lm4_def, type = "HC1", cluster = "time")
robust_def   <- sqrt(diag(cov1))

cov_neut        <- vcovHC(lm4_neut, type = "HC1", cluster = "time")
robust_neut  <- sqrt(diag(cov2))

cov_nonaggr        <- vcovHC(lm4_nonaggr, type = "HC1", cluster = "time")
robust_nonaggr   <- sqrt(diag(cov3))

cov_entente        <- vcovHC(lm4_entente, type = "HC1", cluster = "time")
robust_entente   <- sqrt(diag(cov4))



stargazer(lm4_def, lm4_neut, lm4_nonaggr, lm4_entente,
          multicolumn = F,
          model.numbers= F,
          #add.lines = list(c("Year FE", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
          #                 (c("Sender FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))),
          single.row = F,
          initial.zero = F,
          omit.stat = c( "f", "aic", "bic","chi2","ser", "ll"),
          omit = c("year", "sender")
          ,se = list(robust_def, robust_neut, robust_nonaggr, robust_entente)
)

#sanction onset:


lm1 <- lm(Flow_3t ~ first_sanct
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
          + factor(year) + factor(sender), data = Sanctions)

lm2 <- lm(Flow_3t ~ bq
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
          + factor(year) + factor(sender), data = Sanctions)

lm3 <- lm(Flow_3t ~ first_sanct + bq
          + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
          + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
          + factor(year) + factor(sender), data = Sanctions)

lm4_cap <- lm(Flow_3t ~  bq * first_sanct
              + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
              + Contig + absidealimportantdiff_t3 + logdistance_t3 + all_entente_3t
              + factor(year) + factor(sender) , data = Sanctions)


lm4_pol <- lm(Flow_3t ~ first_sanct * absidealimportantdiff_t3
              + logGDP_t + logGDP_3 + Pop_t + Pop_3 + v2x_polyarchy
              + Contig  + logdistance_t3 +all_entente_3t
              + factor(year) + factor(sender), data = Sanctions)


lm4_alliances <- lm(Flow_3t ~ first_sanct * all_def_3t
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
