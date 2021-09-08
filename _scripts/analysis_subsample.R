library(plm)
library(jtools)
library(sjPlot)
library(sjmisc)
library(ggplot2)
#library(margins.plm)
library(interflex)
library(stargazer)
library(varhandle)
#subsample analysis
# currently available: sample = "US" or sample = "EU"
sample <-"EU"
#If only potential cases, set p = T
p <- F
if (sample == "US"){
  load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/US_Sanct3")
  if (p == T){
    Sanctions <- US_Sanct[US_Sanct$potential_sanction == 1,]
    Sanctions$US[is.na(Sanctions$US)] <- 0
    Sanctions$sanction_dyad <- Sanctions$US
  } else {
    Sanctions <- US_Sanct
    Sanctions$US[is.na(Sanctions$US)] <- 0
    Sanctions$sanction_dyad <- Sanctions$US
    }
  } else if (sample == "EU"){
  load("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_data/EUSANCT/EU_Sanct3")
  if (p == T){
    Sanctions <- EU_Sanct[EU_Sanct$potential_sanction == 1,]
    Sanctions$US[is.na(Sanctions$US)] <- 0
    Sanctions$sanction_dyad <- Sanctions$EU
  } else {
    Sanctions <- EU_Sanct
    Sanctions$EU[is.na(Sanctions$EU)] <- 0
    Sanctions$sanction_dyad <- Sanctions$EU
    }
}
rm(EU_Sanct)
rm(US_Sanct)
sum(!is.na(Sanctions$Flow_s3))
Sanctions <- Sanctions[!is.na(Sanctions$Flow_s3),]
Sanctions <- Sanctions[!is.na(Sanctions$Flow_3s),]
Sanctions <- Sanctions[!is.na(Sanctions$Flow_t3),]
Sanctions <- Sanctions[!is.na(Sanctions$Flow_3t),]
Sanctions <- Sanctions[!is.na(Sanctions$Flow_st),]

Sanctions$Flow_s3 <- Sanctions$Flow_s3 / (sqrt(var(Sanctions$Flow_s3))*2)
Sanctions$Flow_3s <- Sanctions$Flow_3s / (sqrt(var(Sanctions$Flow_3s))*2)
Sanctions$Flow_t3 <- Sanctions$Flow_t3 / (sqrt(var(Sanctions$Flow_t3))*2)
Sanctions$Flow_3t <- Sanctions$Flow_3t / (sqrt(var(Sanctions$Flow_3t))*2)
Sanctions$Flow_st <- Sanctions$Flow_st / (sqrt(var(Sanctions$Flow_st))*2)
#Sanctions$Flow_ts <- Sanctions$Flow_ts / (sqrt(var(Sanctions$Flow_ts))*2)
colnames(Sanctions)[colnames(Sanctions) == "sanction_dyad"] <- "sanct"
Sanctions$bq <- unfactor(Sanctions$bq)
sum(!is.na(Sanctions$Flow_s3))

m_s3_sanct <- lm(Flow_s3 ~ sanct
          + logGDP_s + logGDP_3 + Pop_s + Pop_3
          + absidealimportantdiff_3s + logdist_s3
          + factor(year) + factor(sender), data = Sanctions)

m_s3_bq <- lm(Flow_s3 ~ bq
          + logGDP_s + logGDP_3 + Pop_s + Pop_3
          + absidealimportantdiff_3s + logdist_s3
          + factor(year), data = Sanctions)

m_s3_bq_sanct <- lm(Flow_s3 ~ sanct + bq
          + logGDP_s + logGDP_3 + Pop_s + Pop_3
          + absidealimportantdiff_3s + logdist_s3
          + factor(year), data = Sanctions)

m_s3_bqxsanct <- lm(Flow_s3 ~  bq * sanct
              + logGDP_s + logGDP_3 + Pop_s + Pop_3
              + absidealimportantdiff_3s + logdist_s3
              + factor(year) , data = Sanctions)

m_s3_pol <- lm(Flow_s3 ~ sanct * absidealimportantdiff_3s + bq
              + logGDP_s + logGDP_3 + Pop_s + Pop_3 + v2x_polyarchy
              + Contig  + logdist_s3 
              + factor(year) + factor(sender), data = Sanctions)



cov1       <- vcovHC(m_s3_sanct, type = "HC1", cluster = "time")
robust_se1   <- sqrt(diag(cov1))

cov2         <- vcovHC(m_s3_bq, type = "HC1", cluster = "time")
robust_se2  <- sqrt(diag(cov2))

cov3         <- vcovHC(m_s3_bq_sanct, type = "HC1", cluster = "time")
robust_se3   <- sqrt(diag(cov3))

cov4        <- vcovHC(m_s3_bqxsanct, type = "HC1", cluster = "time")
robust_se4   <- sqrt(diag(cov4))


cov5        <- vcovHC(m_s3_pol, type = "HC1", cluster = "time")
robust_se5   <- sqrt(diag(cov5))


stargazer(m_s3_sanct, m_s3_bq, m_s3_bq_sanct, m_s3_bqxsanct, m_s3_pol,
          multicolumn = F,
          model.numbers= F,
          #add.lines = list(c("Year FE", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
          #                 (c("Sender FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))),
          single.row = F,
          initial.zero = F,
          omit.stat = c( "f", "aic", "bic","chi2","ser", "ll"),
          omit = c("year", "sender")
          ,se = list(robust_se1, robust_se2, robust_se3, robust_se4, robust_se5)
)



title = paste0("Predicted trade from ", sample, " to third country, effect of Bureaucratic quality and sanctions")
plot_model(m_s3_bqxsanct, type = "pred", terms = c("bq", "sanct"),
           title = title,
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Sanction",
           vcov.type = "HC1")

m_st_bqxsanct <- lm(Flow_st ~  bq * sanct
              + logGDP_s + logGDP_3 + Pop_s + Pop_3
              + absidealimportantdiff_3s + logdist_s3
              + factor(year) , data = Sanctions)

#summary(m_st_bqxsanct)

title = paste0("Predicted trade from ", sample, " to target, effect of Bureaucratic quality and sanctions")
plot_model(m_st_bqxsanct, type = "pred", terms = c("bq", "sanct"),
           title = title,
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Sanction",
           vcov.type = "HC1")


####from sender to third country

m_s3_sanct <- lm(Flow_s3 ~ sanct
                 + logGDP_s + logGDP_3 + Pop_s + Pop_3
                 + absidealimportantdiff_3s + logdist_s3
                 + factor(year) + factor(sender), data = Sanctions)

m_s3_bq <- lm(Flow_s3 ~ bq
              + logGDP_s + logGDP_3 + Pop_s + Pop_3
              + absidealimportantdiff_3s + logdist_s3
              + factor(year), data = Sanctions)

m_s3_bq_sanct <- lm(Flow_s3 ~ sanct + bq
                    + logGDP_s + logGDP_3 + Pop_s + Pop_3
                    + absidealimportantdiff_3s + logdist_s3
                    + factor(year), data = Sanctions)

m_s3_bqxsanct <- lm(Flow_s3 ~  bq * sanct
                    + logGDP_s + logGDP_3 + Pop_s + Pop_3
                    + absidealimportantdiff_3s + logdist_s3
                    + factor(year) , data = Sanctions)

cov1       <- vcovHC(m_s3_sanct, type = "HC1", cluster = "time")
robust_se1   <- sqrt(diag(cov1))

cov2         <- vcovHC(m_s3_bq, type = "HC1", cluster = "time")
robust_se2  <- sqrt(diag(cov2))

cov3         <- vcovHC(m_s3_bq_sanct, type = "HC1", cluster = "time")
robust_se3   <- sqrt(diag(cov3))

cov4        <- vcovHC(m_s3_bqxsanct, type = "HC1", cluster = "time")
robust_se5   <- sqrt(diag(cov4))


title = paste0("Predicted trade from ", sample, " to third country, effect of Bureaucratic quality and sanctions")
plot_model(m_s3_bqxsanct, type = "pred", terms = c("bq", "sanct"),
           title = title,
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Sanction",
           vcov.type = "HC1")

m_st_bqxsanct <- lm(Flow_st ~  bq * sanct
                    + logGDP_s + logGDP_3 + Pop_s + Pop_3
                    + absidealimportantdiff_3s + logdist_s3
                    + factor(year) , data = Sanctions)

#summary(m_st_bqxsanct)

title = paste0("Predicted trade from ", sample, " to target, effect of Bureaucratic quality and sanctions")
plot_model(m_st_bqxsanct, type = "pred", terms = c("bq", "sanct"),
           title = title,
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Sanction",
           vcov.type = "HC1")



m_3t_bqxsanct <- lm(Flow_3t ~  bq * sanct
                    + logGDP_s + logGDP_3 + Pop_s + Pop_3
                    + absidealimportantdiff_3s + logdist_s3
                    + factor(year) , data = Sanctions)



title = paste0("Predicted trade from third country to target, effect of Bureaucratic quality and sanctions (", sample," sample)")

plot_model(m_3t_bqxsanct , type = "pred", terms = c("bq", "sanct"),
           title = title,
           axis.title = c("Bureaucratic quality (bq)", "Normalized trade flows"),
           legend.title = "Sanction",
           vcov.type = "HC1")





Sanctions <- Sanctions[Sanctions$sanct == 1,]

m_s3_3t <- lm(Flow_s3 ~ Flow_3t * bq
              + logGDP_s + logGDP_3 + logGDP_t + Pop_s + Pop_3 + Pop_t
              + absidealimportantdiff_3s + logdist_s3 + logdist_t3 + absidealimportantdiff_t3
              + factor(year), data = Sanctions)

summary(m_s3_3t)

title = paste0("Predicted trade from third country to target, effect of Bureaucratic quality and third country to target trade (", sample," sample)")

plot_model(m_s3_3t , type = "pred", terms = c("Flow_3t", "bq"),
           title = title,
           axis.title = c("Normalized trade flows, third country to target", "Normalized trade flows, sender to third country"),
           legend.title = "bq",
           vcov.type = "HC1")
