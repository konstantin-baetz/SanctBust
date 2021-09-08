#predictive modelling

library(mlr3)
library(Metrics)
library(plm)
library(jtools)
library(sjPlot)
library(sjmisc)
library(dplyr)
library(ggplot2)
#library(interflex)
library(stargazer)
library(varhandle)
library(purrr)
library(rpart)
library(rattle)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
library(MLmetrics)
library(ggplot2)
source("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_scripts/functions.R")
theme_update(text = element_text(size=20))

##define what to run:

#actual sanction cases or all observations as basis for prediction?

#obs <- "allobs"
obs <- "sancts"



# can specifiy which sanctions data to load. Options:
#"EU", "UN", "US";
#potential sanctions only: T, F
samples <- c("UN", "US", "EU")

for (sample in samples){
#load
Sanctions <- sanct_load(sample, T)
colnames(Sanctions)[colnames(Sanctions) == "sanction_dyad"] <- "sanct"
Sanctions$bq <- unfactor(Sanctions$bq)
#clean
Sanctions <- Sanctions[!is.na(Sanctions$Flow_3t),]
Sanctions <- Sanctions[order(Sanctions$year),]
Sanctions$triad <- paste(Sanctions$sender, Sanctions$target_name, Sanctions$thrd_ctry_name, sep = "_")
Sanctions <- Sanctions %>% group_by(triad) %>% mutate(Flow_3t_tp1 = dplyr::lead(Flow_3t))
Sanctions <- ungroup(Sanctions)
#select vars
Sanctions <- dplyr::select(Sanctions, Flow_3t, Flow_t3, Flow_3t_tp1, year,
                           bq , sanct, logGDP_t, logGDP_3, Pop_t, Pop_3, v2x_polyarchy,
                           Contig, absidealimportantdiff_t3, logdistance_t3, all_entente_3t, all_def_3t)

#Sanctions$bq_x_sanct <- Sanctions$bq * Sanctions$sanct
#Sanctions$un_x_sanct <- Sanctions$absidealimportantdiff_t3 * Sanctions$sanct

if (obs == "allobs"){
  

#define functions for regressions and random forests

rf_simple <- (Flow_3t_tp1 ~ Flow_3t +
                sanct + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
                   v2x_polyarchy + Contig +  logdistance_t3 )

rf_bq <- (Flow_3t_tp1 ~ Flow_3t + bq_x_sanct +  bq + sanct + logGDP_t + logGDP_3 + Pop_t + 
            Pop_3 + v2x_polyarchy + Contig +
            logdistance_t3)

rf_un <- (Flow_3t_tp1 ~ Flow_3t + un_x_sanct + absidealimportantdiff_t3 + sanct + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
            v2x_polyarchy + Contig  + logdistance_t3  )

rf_full <- (Flow_3t_tp1 ~ Flow_3t + un_x_sanct + bq_x_sanct + absidealimportantdiff_t3 + sanct + bq + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
              v2x_polyarchy + Contig + logdistance_t3 )

reg_simple <- (Flow_3t_tp1 ~ Flow_3t +
                sanct + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
                v2x_polyarchy + Contig + logdistance_t3 )

reg_bq <- (Flow_3t_tp1 ~ Flow_3t +  bq * sanct + logGDP_t + logGDP_3 + Pop_t + 
            Pop_3 + v2x_polyarchy + Contig +
            logdistance_t3 )

reg_un <- (Flow_3t_tp1 ~ Flow_3t + absidealimportantdiff_t3 * sanct + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
            v2x_polyarchy + Contig  + logdistance_t3 )

reg_full <- (Flow_3t_tp1 ~ Flow_3t + absidealimportantdiff_t3 * sanct + bq * sanct + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
              v2x_polyarchy + Contig + logdistance_t3 )


#define training and test data

years <- (2009:2013)

for (year in years){
  train_reg <- Sanctions[Sanctions$year < year,]
  test_reg <- Sanctions[Sanctions$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "simple model"))
  print(Sys.time())
  
  #running models without bq
  m_simple_reg <- lm(reg_simple, data = train_reg)
  m_simple_rf <- ranger(rf_simple,
                        data = train_rf,
                        num.trees = 500,
                        respect.unordered.factors = "order",
                        importance = 'impurity')
  var_imp_simple_rf <- m_simple_rf$variable.importance
  var_imp_simple_rf_scaled <- var_imp_simple_rf/max(var_imp_simple_rf)
  var_imp_simple_rf_name <- paste0("feature_importance_simple", year)
  assign(var_imp_simple_rf_name, var_imp_simple_rf_scaled)
  
  test_reg$pred_reg <- predict(m_simple_reg, newdata = test_reg)
  eval_reg_simple <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_simple_rf, test_rf)$predictions
  eval_rf_simple <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_simple <- na.omit(eval_reg_simple)
  eval_reg_simple$y_hat <- log(eval_reg_simple$pred_reg)
  eval_reg_simple$y <- log(eval_reg_simple$Flow_3t_tp1)
  eval_reg_simple$year <- year
  eval_reg_simple <- na.omit(eval_reg_simple)
  eval_reg_simple <- eval_reg_simple[eval_reg_simple$y_hat >= 0,]
  eval_reg_simple <- eval_reg_simple[eval_reg_simple$y >= 0,]
  
  eval_rf_simple <- na.omit(eval_rf_simple)
  eval_rf_simple$y_hat <- log(eval_rf_simple$pred_rf)
  eval_rf_simple$y <- log(eval_rf_simple$Flow_3t_tp1)
  eval_rf_simple$year <- year
  eval_rf_simple <- na.omit(eval_rf_simple)
  eval_rf_simple <- eval_rf_simple[eval_rf_simple$y_hat >= 0,]
  eval_rf_simple <- eval_rf_simple[eval_rf_simple$y >= 0,]
  
  
  if(year == years[1]){
    eval_reg_simple_full <- eval_reg_simple
    eval_rf_simple_full <- eval_rf_simple
  } else {
    eval_reg_simple_full <- rbind(eval_reg_simple_full, eval_reg_simple)
    eval_rf_simple_full <- rbind(eval_rf_simple_full, eval_rf_simple)
  }
  
  #running models with bq
  train_reg <- Sanctions[Sanctions$year < year,]
  test_reg <- Sanctions[Sanctions$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "bq model"))
  print(Sys.time())
  m_bq_reg <- lm(reg_bq, data  = train_reg)
  m_bq_rf <- ranger(rf_bq,
                    data = train_rf,
                    num.trees = 500,
                    respect.unordered.factors = "order",
                    importance = 'impurity')
  
  var_imp_bq_rf <- m_bq_rf$variable.importance
  var_imp_bq_rf_scaled <- var_imp_bq_rf/max(var_imp_bq_rf)
  var_imp_bq_rf_name <- paste0("feature_importance_bq", year)
  assign(var_imp_bq_rf_name, var_imp_bq_rf_scaled)
  
  test_reg$pred_reg <- predict(m_bq_reg, newdata = test_reg)
  eval_reg_bq <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_bq_rf, test_rf)$predictions
  eval_rf_bq <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_bq <- na.omit(eval_reg_bq)
  eval_reg_bq$y_hat <- log(eval_reg_bq$pred_reg)
  eval_reg_bq$y <- log(eval_reg_bq$Flow_3t_tp1)
  eval_reg_bq$year <- year
  eval_reg_bq <- na.omit(eval_reg_bq)
  eval_reg_bq <- eval_reg_bq[eval_reg_bq$y_hat >= 0,]
  eval_reg_bq <- eval_reg_bq[eval_reg_bq$y >= 0,]
  
  eval_rf_bq <- na.omit(eval_rf_bq)
  eval_rf_bq$y_hat <- log(eval_rf_bq$pred_rf)
  eval_rf_bq$y <- log(eval_rf_bq$Flow_3t_tp1)
  eval_rf_bq$year <- year
  eval_rf_bq <- na.omit(eval_rf_bq)
  eval_rf_bq <- eval_rf_bq[eval_rf_bq$y_hat >= 0,]
  eval_rf_bq <- eval_rf_bq[eval_rf_bq$y >= 0,]
  
  if(year == years[1]){
    eval_reg_bq_full <- eval_reg_bq
    eval_rf_bq_full <- eval_rf_bq
  } else {
    eval_reg_bq_full <- rbind(eval_reg_bq_full, eval_reg_bq)
    eval_rf_bq_full <- rbind(eval_rf_bq_full, eval_rf_bq)
  }
  
  #running models with unga difference
  train_reg <- Sanctions[Sanctions$year < year,]
  test_reg <- Sanctions[Sanctions$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "un diff model"))
  print(Sys.time())
  m_un_reg <- lm(reg_un, data  = train_reg)
  m_un_rf <- ranger(rf_un,
                    data = train_rf,
                    num.trees = 500,
                    respect.unordered.factors = "order",
                    importance = 'impurity')
  
  var_imp_un_rf <- m_un_rf$variable.importance
  var_imp_un_rf_scaled <- var_imp_un_rf/max(var_imp_un_rf)
  var_imp_un_rf_name <- paste0("feature_importance_un", year)
  assign(var_imp_un_rf_name, var_imp_un_rf_scaled)
  
  test_reg$pred_reg <- predict(m_un_reg, newdata = test_reg)
  eval_reg_un <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_un_rf, test_rf)$predictions
  eval_rf_un <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_un <- na.omit(eval_reg_un)
  eval_reg_un$y_hat <- log(eval_reg_un$pred_reg)
  eval_reg_un$y <- log(eval_reg_un$Flow_3t_tp1)
  eval_reg_un$year <- year
  eval_reg_un <- na.omit(eval_reg_un)
  eval_reg_un <- eval_reg_un[eval_reg_un$y_hat >= 0,]
  eval_reg_un <- eval_reg_un[eval_reg_un$y >= 0,]
  
  eval_rf_un <- na.omit(eval_rf_un)
  eval_rf_un$y_hat <- log(eval_rf_un$pred_rf)
  eval_rf_un$y <- log(eval_rf_un$Flow_3t_tp1)
  eval_rf_un$year <- year
  eval_rf_un <- na.omit(eval_rf_un)
  eval_rf_un <- eval_rf_un[eval_rf_un$y_hat >= 0,]
  eval_rf_un <- eval_rf_un[eval_rf_un$y >= 0,]
  
  if(year == years[1]){
    eval_reg_un_full <- eval_reg_un
    eval_rf_un_full <- eval_rf_un
  } else {
    eval_reg_un_full <- rbind(eval_reg_un_full, eval_reg_un)
    eval_rf_un_full <- rbind(eval_rf_un_full, eval_rf_un)
  }
  
  #running full model
  
  
  #running models with unga difference
  train_reg <- Sanctions[Sanctions$year < year,]
  test_reg <- Sanctions[Sanctions$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "full model"))
  print(Sys.time())
  m_full_reg <- lm(reg_full, data  = train_reg)
  m_full_rf <- ranger(rf_full,
                      data = train_rf,
                      num.trees = 500,
                      respect.unordered.factors = "order",
                      importance = 'impurity')
  
  var_imp_full_rf <- m_full_rf$variable.importance
  var_imp_full_rf_scaled <- var_imp_full_rf/max(var_imp_full_rf)
  var_imp_full_rf_name <- paste0("feature_importance_full", year)
  assign(var_imp_full_rf_name, var_imp_full_rf_scaled)
  
  test_reg$pred_reg <- predict(m_full_reg, newdata = test_reg)
  eval_reg_full <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_full_rf, test_rf)$predictions
  eval_rf_full <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_full <- na.omit(eval_reg_full)
  eval_reg_full$y_hat <- log(eval_reg_full$pred_reg)
  eval_reg_full$y <- log(eval_reg_full$Flow_3t_tp1)
  eval_reg_full$year <- year
  eval_reg_full <- na.omit(eval_reg_full)
  eval_reg_full <- eval_reg_full[eval_reg_full$y_hat >= 0,]
  eval_reg_full <- eval_reg_full[eval_reg_full$y >= 0,]
  
  eval_rf_full <- na.omit(eval_rf_full)
  eval_rf_full$y_hat <- log(eval_rf_full$pred_rf)
  eval_rf_full$y <- log(eval_rf_full$Flow_3t_tp1)
  eval_rf_full$year <- year
  eval_rf_full <- na.omit(eval_rf_full)
  eval_rf_full <- eval_rf_full[eval_rf_full$y_hat >= 0,]
  eval_rf_full <- eval_rf_full[eval_rf_full$y >= 0,]
  
  if(year == years[1]){
    eval_reg_full_full <- eval_reg_full
    eval_rf_full_full <- eval_rf_full
  } else {
    eval_reg_full_full <- rbind(eval_reg_full_full, eval_reg_full)
    eval_rf_full_full <- rbind(eval_rf_full_full, eval_rf_full)
  }  
}

eval_reg_simple_2009 <- eval_reg_simple_full[eval_reg_simple_full$year == 2009,]
eval_reg_simple_2010 <- eval_reg_simple_full[eval_reg_simple_full$year == 2010,]
eval_reg_simple_2011 <- eval_reg_simple_full[eval_reg_simple_full$year == 2011,]
eval_reg_simple_2012 <- eval_reg_simple_full[eval_reg_simple_full$year == 2012,]
eval_reg_simple_2013 <- eval_reg_simple_full[eval_reg_simple_full$year == 2013,]


eval_reg_bq_2009 <- eval_reg_bq_full[eval_reg_bq_full$year == 2009,]
eval_reg_bq_2010 <- eval_reg_bq_full[eval_reg_bq_full$year == 2010,]
eval_reg_bq_2011 <- eval_reg_bq_full[eval_reg_bq_full$year == 2011,]
eval_reg_bq_2012 <- eval_reg_bq_full[eval_reg_bq_full$year == 2012,]
eval_reg_bq_2013 <- eval_reg_bq_full[eval_reg_bq_full$year == 2013,]

eval_reg_un_2009 <- eval_reg_un_full[eval_reg_un_full$year == 2009,]
eval_reg_un_2010 <- eval_reg_un_full[eval_reg_un_full$year == 2010,]
eval_reg_un_2011 <- eval_reg_un_full[eval_reg_un_full$year == 2011,]
eval_reg_un_2012 <- eval_reg_un_full[eval_reg_un_full$year == 2012,]
eval_reg_un_2013 <- eval_reg_un_full[eval_reg_un_full$year == 2013,]

eval_reg_full_2009 <- eval_reg_full_full[eval_reg_full_full$year == 2009,]
eval_reg_full_2010 <- eval_reg_full_full[eval_reg_full_full$year == 2010,]
eval_reg_full_2011 <- eval_reg_full_full[eval_reg_full_full$year == 2011,]
eval_reg_full_2012 <- eval_reg_full_full[eval_reg_full_full$year == 2012,]
eval_reg_full_2013 <- eval_reg_full_full[eval_reg_full_full$year == 2013,]


eval_rf_simple_2009 <- eval_rf_simple_full[eval_rf_simple_full$year == 2009,]
eval_rf_simple_2010 <- eval_rf_simple_full[eval_rf_simple_full$year == 2010,]
eval_rf_simple_2011 <- eval_rf_simple_full[eval_rf_simple_full$year == 2011,]
eval_rf_simple_2012 <- eval_rf_simple_full[eval_rf_simple_full$year == 2012,]
eval_rf_simple_2013 <- eval_rf_simple_full[eval_rf_simple_full$year == 2013,]


eval_rf_bq_2009 <- eval_rf_bq_full[eval_rf_bq_full$year == 2009,]
eval_rf_bq_2010 <- eval_rf_bq_full[eval_rf_bq_full$year == 2010,]
eval_rf_bq_2011 <- eval_rf_bq_full[eval_rf_bq_full$year == 2011,]
eval_rf_bq_2012 <- eval_rf_bq_full[eval_rf_bq_full$year == 2012,]
eval_rf_bq_2013 <- eval_rf_bq_full[eval_rf_bq_full$year == 2013,]

eval_rf_un_2009 <- eval_rf_un_full[eval_rf_un_full$year == 2009,]
eval_rf_un_2010 <- eval_rf_un_full[eval_rf_un_full$year == 2010,]
eval_rf_un_2011 <- eval_rf_un_full[eval_rf_un_full$year == 2011,]
eval_rf_un_2012 <- eval_rf_un_full[eval_rf_un_full$year == 2012,]
eval_rf_un_2013 <- eval_rf_un_full[eval_rf_un_full$year == 2013,]


eval_rf_full_2009 <- eval_rf_full_full[eval_rf_full_full$year == 2009,]
eval_rf_full_2010 <- eval_rf_full_full[eval_rf_full_full$year == 2010,]
eval_rf_full_2011 <- eval_rf_full_full[eval_rf_full_full$year == 2011,]
eval_rf_full_2012 <- eval_rf_full_full[eval_rf_full_full$year == 2012,]
eval_rf_full_2013 <- eval_rf_full_full[eval_rf_full_full$year == 2013,]



mse_reg_simple <- MSE(eval_reg_simple_full$y_hat, eval_reg_simple_full$y)
mse_rf_simple <- MSE(eval_rf_simple_full$y_hat, eval_rf_simple_full$y)

mse_reg_bq <- MSE(eval_rf_bq_full$y_hat, eval_rf_bq_full$y)
mse_rf_bq <- MSE(eval_rf_bq_full$y_hat, eval_rf_bq_full$y)

mse_reg_un <- MSE(eval_rf_un_full$y_hat, eval_rf_un_full$y)
mse_rf_un <- MSE(eval_rf_un_full$y_hat, eval_rf_un_full$y)


importances_bq <- c(feature_importance_bq2009[2], feature_importance_bq2010[2], 
                 feature_importance_bq2011[2], feature_importance_bq2012[2],
                 feature_importance_bq2013[2])

importances_un <- c(feature_importance_un2009[2], feature_importance_un2010[2], 
                    feature_importance_un2011[2], feature_importance_un2012[2],
                    feature_importance_un2013[2])


mse_reg_simple_yearly <- c(MSE(eval_reg_simple_2009$y_hat, eval_reg_simple_2009$y),
                      MSE(eval_reg_simple_2010$y_hat, eval_reg_simple_2010$y),
                      MSE(eval_reg_simple_2011$y_hat, eval_reg_simple_2011$y),
                      MSE(eval_reg_simple_2012$y_hat, eval_reg_simple_2012$y),
                      MSE(eval_reg_simple_2013$y_hat, eval_reg_simple_2013$y))


mse_reg_bq_yearly <- c(MSE(eval_reg_bq_2009$y_hat, eval_reg_bq_2009$y),
                      MSE(eval_reg_bq_2010$y_hat, eval_reg_bq_2010$y),
                      MSE(eval_reg_bq_2011$y_hat, eval_reg_bq_2011$y),
                      MSE(eval_reg_bq_2012$y_hat, eval_reg_bq_2012$y),
                      MSE(eval_reg_bq_2013$y_hat, eval_reg_bq_2013$y))

mse_reg_un_yearly <- c(MSE(eval_reg_un_2009$y_hat, eval_reg_un_2009$y),
                       MSE(eval_reg_un_2010$y_hat, eval_reg_un_2010$y),
                       MSE(eval_reg_un_2011$y_hat, eval_reg_un_2011$y),
                       MSE(eval_reg_un_2012$y_hat, eval_reg_un_2012$y),
                       MSE(eval_reg_un_2013$y_hat, eval_reg_un_2013$y))


mse_reg_full_yearly <- c(MSE(eval_reg_full_2009$y_hat, eval_reg_full_2009$y),
                         MSE(eval_reg_full_2010$y_hat, eval_reg_full_2010$y),
                         MSE(eval_reg_full_2011$y_hat, eval_reg_full_2011$y),
                         MSE(eval_reg_full_2012$y_hat, eval_reg_full_2012$y),
                         MSE(eval_reg_full_2013$y_hat, eval_reg_full_2013$y))


mse_rf_simple_yearly <- c(MSE(eval_rf_simple_2009$y_hat, eval_rf_simple_2009$y),
                      MSE(eval_rf_simple_2010$y_hat, eval_rf_simple_2010$y),
                      MSE(eval_rf_simple_2011$y_hat, eval_rf_simple_2011$y),
                      MSE(eval_rf_simple_2012$y_hat, eval_rf_simple_2012$y),
                      MSE(eval_rf_simple_2013$y_hat, eval_rf_simple_2013$y))


mse_rf_bq_yearly <- c(MSE(eval_rf_bq_2009$y_hat, eval_rf_bq_2009$y),
                      MSE(eval_rf_bq_2010$y_hat, eval_rf_bq_2010$y),
                      MSE(eval_rf_bq_2011$y_hat, eval_rf_bq_2011$y),
                      MSE(eval_rf_bq_2012$y_hat, eval_rf_bq_2012$y),
                      MSE(eval_rf_bq_2013$y_hat, eval_rf_bq_2013$y))

mse_rf_un_yearly <- c(MSE(eval_rf_un_2009$y_hat, eval_rf_un_2009$y),
                      MSE(eval_rf_un_2010$y_hat, eval_rf_un_2010$y),
                      MSE(eval_rf_un_2011$y_hat, eval_rf_un_2011$y),
                      MSE(eval_rf_un_2012$y_hat, eval_rf_un_2012$y),
                      MSE(eval_rf_un_2013$y_hat, eval_rf_un_2013$y))

mse_rf_full_yearly <- c(MSE(eval_rf_full_2009$y_hat, eval_rf_full_2009$y),
                        MSE(eval_rf_full_2010$y_hat, eval_rf_full_2010$y),
                        MSE(eval_rf_full_2011$y_hat, eval_rf_full_2011$y),
                        MSE(eval_rf_full_2012$y_hat, eval_rf_full_2012$y),
                        MSE(eval_rf_full_2013$y_hat, eval_rf_full_2013$y))


comp <- as.data.frame(cbind(years,
                               importances_bq,
                               importances_un,
                               mse_reg_simple_yearly,
                               mse_reg_bq_yearly,
                               mse_reg_un_yearly,
                               mse_reg_full_yearly,
                               mse_rf_simple_yearly,
                               mse_rf_bq_yearly,
                               mse_rf_un_yearly,
                               mse_rf_full_yearly))

comp$years <- years+1


Perf_all_cases<- ggplot(data = comp, aes(x = years)) +
  geom_line(aes(y = mse_reg_simple_yearly, col = "regression baseline "), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_reg_bq_yearly, col ="regression with state capacity" ), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_reg_un_yearly, col ="regression with political alignment" ), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_reg_full_yearly, col ="regression full model" ), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_rf_simple_yearly, col ="random forest baseline" ), size = 1.5) +
  geom_line(aes(y = mse_rf_bq_yearly, col ="random forest with state capacity" ), size = 1.5) +
  geom_line(aes(y = mse_rf_un_yearly, col ="random forest with political alignment" ), size = 1.5) +
  geom_line(aes(y = mse_rf_full_yearly, col ="random forest full model" ), size = 1.5) +
  labs(title="Predictive performance by year predicted", 
       subtitle="Mean squared error in predicting trade flows from third country to target",
       y="MSE",
       x="year predicted",
       color = "Model")

Perf_all_cases

} else if (obs == "sancts") {

#a dataset of only sanctions:
Sanctions_only <- Sanctions[Sanctions$sanct == 1,]


#define functions for regressions and random forests

rf_simple <- (Flow_3t_tp1 ~ Flow_3t + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
                v2x_polyarchy + Contig +  logdistance_t3 )

rf_bq <- (Flow_3t_tp1 ~ Flow_3t +  bq  + logGDP_t + logGDP_3 + Pop_t + 
            Pop_3 + v2x_polyarchy + Contig +
            logdistance_t3)

rf_un <- (Flow_3t_tp1 ~ Flow_3t  + absidealimportantdiff_t3  + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
            v2x_polyarchy + Contig  + logdistance_t3  )

rf_full <- (Flow_3t_tp1 ~ Flow_3t + absidealimportantdiff_t3 + bq + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
              v2x_polyarchy + Contig + logdistance_t3 )

reg_simple <- (Flow_3t_tp1 ~ Flow_3t + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
                 v2x_polyarchy + Contig + logdistance_t3 )

reg_bq <- (Flow_3t_tp1 ~ Flow_3t +  bq  + logGDP_t + logGDP_3 + Pop_t + 
             Pop_3 + v2x_polyarchy + Contig +
             logdistance_t3 )

reg_un <- (Flow_3t_tp1 ~ Flow_3t + absidealimportantdiff_t3  + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
             v2x_polyarchy + Contig  + logdistance_t3 )

reg_full <- (Flow_3t_tp1 ~ Flow_3t + absidealimportantdiff_t3  + bq + logGDP_t + logGDP_3 + Pop_t + Pop_3 +
               v2x_polyarchy + Contig + logdistance_t3 )


#define training and test data

years <- (2009:2013)

for (year in years){
  train_reg <- Sanctions_only[Sanctions_only$year < year,]
  test_reg <- Sanctions_only[Sanctions_only$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "simple model"))
  print(Sys.time())
  
  #running models without bq
  m_simple_reg <- lm(reg_simple, data = train_reg)
  m_simple_rf <- ranger(rf_simple,
                        data = train_rf,
                        num.trees = 500,
                        respect.unordered.factors = "order",
                        importance = 'impurity')
  var_imp_simple_rf <- m_simple_rf$variable.importance
  var_imp_simple_rf_scaled <- var_imp_simple_rf/max(var_imp_simple_rf)
  var_imp_simple_rf_name <- paste0("feature_importance_simple", year)
  assign(var_imp_simple_rf_name, var_imp_simple_rf_scaled)
  
  test_reg$pred_reg <- predict(m_simple_reg, newdata = test_reg)
  eval_reg_simple <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_simple_rf, test_rf)$predictions
  eval_rf_simple <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_simple <- na.omit(eval_reg_simple)
  eval_reg_simple$y_hat <- log(eval_reg_simple$pred_reg)
  eval_reg_simple$y <- log(eval_reg_simple$Flow_3t_tp1)
  eval_reg_simple$year <- year
  eval_reg_simple <- na.omit(eval_reg_simple)
  eval_reg_simple <- eval_reg_simple[eval_reg_simple$y_hat >= 0,]
  eval_reg_simple <- eval_reg_simple[eval_reg_simple$y >= 0,]
  
  eval_rf_simple <- na.omit(eval_rf_simple)
  eval_rf_simple$y_hat <- log(eval_rf_simple$pred_rf)
  eval_rf_simple$y <- log(eval_rf_simple$Flow_3t_tp1)
  eval_rf_simple$year <- year
  eval_rf_simple <- na.omit(eval_rf_simple)
  eval_rf_simple <- eval_rf_simple[eval_rf_simple$y_hat >= 0,]
  eval_rf_simple <- eval_rf_simple[eval_rf_simple$y >= 0,]
  
  
  if(year == years[1]){
    eval_reg_simple_full <- eval_reg_simple
    eval_rf_simple_full <- eval_rf_simple
  } else {
    eval_reg_simple_full <- rbind(eval_reg_simple_full, eval_reg_simple)
    eval_rf_simple_full <- rbind(eval_rf_simple_full, eval_rf_simple)
  }
  
  #running models with bq
  train_reg <- Sanctions_only[Sanctions_only$year < year,]
  test_reg <- Sanctions_only[Sanctions_only$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "bq model"))
  print(Sys.time())
  m_bq_reg <- lm(reg_bq, data  = train_reg)
  m_bq_rf <- ranger(rf_bq,
                    data = train_rf,
                    num.trees = 500,
                    respect.unordered.factors = "order",
                    importance = 'impurity')
  
  var_imp_bq_rf <- m_bq_rf$variable.importance
  var_imp_bq_rf_scaled <- var_imp_bq_rf/max(var_imp_bq_rf)
  var_imp_bq_rf_name <- paste0("feature_importance_bq", year)
  assign(var_imp_bq_rf_name, var_imp_bq_rf_scaled)
  
  test_reg$pred_reg <- predict(m_bq_reg, newdata = test_reg)
  eval_reg_bq <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_bq_rf, test_rf)$predictions
  eval_rf_bq <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_bq <- na.omit(eval_reg_bq)
  eval_reg_bq$y_hat <- log(eval_reg_bq$pred_reg)
  eval_reg_bq$y <- log(eval_reg_bq$Flow_3t_tp1)
  eval_reg_bq$year <- year
  eval_reg_bq <- na.omit(eval_reg_bq)
  eval_reg_bq <- eval_reg_bq[eval_reg_bq$y_hat >= 0,]
  eval_reg_bq <- eval_reg_bq[eval_reg_bq$y >= 0,]
  
  eval_rf_bq <- na.omit(eval_rf_bq)
  eval_rf_bq$y_hat <- log(eval_rf_bq$pred_rf)
  eval_rf_bq$y <- log(eval_rf_bq$Flow_3t_tp1)
  eval_rf_bq$year <- year
  eval_rf_bq <- na.omit(eval_rf_bq)
  eval_rf_bq <- eval_rf_bq[eval_rf_bq$y_hat >= 0,]
  eval_rf_bq <- eval_rf_bq[eval_rf_bq$y >= 0,]
  
  if(year == years[1]){
    eval_reg_bq_full <- eval_reg_bq
    eval_rf_bq_full <- eval_rf_bq
  } else {
    eval_reg_bq_full <- rbind(eval_reg_bq_full, eval_reg_bq)
    eval_rf_bq_full <- rbind(eval_rf_bq_full, eval_rf_bq)
  }
  
  #running models with unga difference
  train_reg <- Sanctions_only[Sanctions_only$year < year,]
  test_reg <- Sanctions_only[Sanctions_only$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "un diff model"))
  print(Sys.time())
  m_un_reg <- lm(reg_un, data  = train_reg)
  m_un_rf <- ranger(rf_un,
                    data = train_rf,
                    num.trees = 500,
                    respect.unordered.factors = "order",
                    importance = 'impurity')
  
  var_imp_un_rf <- m_un_rf$variable.importance
  var_imp_un_rf_scaled <- var_imp_un_rf/max(var_imp_un_rf)
  var_imp_un_rf_name <- paste0("feature_importance_un", year)
  assign(var_imp_un_rf_name, var_imp_un_rf_scaled)
  
  test_reg$pred_reg <- predict(m_un_reg, newdata = test_reg)
  eval_reg_un <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_un_rf, test_rf)$predictions
  eval_rf_un <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_un <- na.omit(eval_reg_un)
  eval_reg_un$y_hat <- log(eval_reg_un$pred_reg)
  eval_reg_un$y <- log(eval_reg_un$Flow_3t_tp1)
  eval_reg_un$year <- year
  eval_reg_un <- na.omit(eval_reg_un)
  eval_reg_un <- eval_reg_un[eval_reg_un$y_hat >= 0,]
  eval_reg_un <- eval_reg_un[eval_reg_un$y >= 0,]
  
  eval_rf_un <- na.omit(eval_rf_un)
  eval_rf_un$y_hat <- log(eval_rf_un$pred_rf)
  eval_rf_un$y <- log(eval_rf_un$Flow_3t_tp1)
  eval_rf_un$year <- year
  eval_rf_un <- na.omit(eval_rf_un)
  eval_rf_un <- eval_rf_un[eval_rf_un$y_hat >= 0,]
  eval_rf_un <- eval_rf_un[eval_rf_un$y >= 0,]
  
  if(year == years[1]){
    eval_reg_un_full <- eval_reg_un
    eval_rf_un_full <- eval_rf_un
  } else {
    eval_reg_un_full <- rbind(eval_reg_un_full, eval_reg_un)
    eval_rf_un_full <- rbind(eval_rf_un_full, eval_rf_un)
  }
  
  #running full model
  
  
  #running models with unga difference
  train_reg <- Sanctions_only[Sanctions_only$year < year,]
  test_reg <- Sanctions_only[Sanctions_only$year == year,]
  train_rf <- na.omit(train_reg)
  test_rf <- na.omit(test_reg)
  print(paste("Running iteration for year", year, "full model"))
  print(Sys.time())
  m_full_reg <- lm(reg_full, data  = train_reg)
  m_full_rf <- ranger(rf_full,
                      data = train_rf,
                      num.trees = 500,
                      respect.unordered.factors = "order",
                      importance = 'impurity')
  
  var_imp_full_rf <- m_full_rf$variable.importance
  var_imp_full_rf_scaled <- var_imp_full_rf/max(var_imp_full_rf)
  var_imp_full_rf_name <- paste0("feature_importance_full", year)
  assign(var_imp_full_rf_name, var_imp_full_rf_scaled)
  
  test_reg$pred_reg <- predict(m_full_reg, newdata = test_reg)
  eval_reg_full <- select(test_reg, pred_reg, Flow_3t_tp1)
  
  test_rf$pred_rf <- predict(m_full_rf, test_rf)$predictions
  eval_rf_full <- select(test_rf, pred_rf, Flow_3t_tp1)
  
  eval_reg_full <- na.omit(eval_reg_full)
  eval_reg_full$y_hat <- log(eval_reg_full$pred_reg)
  eval_reg_full$y <- log(eval_reg_full$Flow_3t_tp1)
  eval_reg_full$year <- year
  eval_reg_full <- na.omit(eval_reg_full)
  eval_reg_full <- eval_reg_full[eval_reg_full$y_hat >= 0,]
  eval_reg_full <- eval_reg_full[eval_reg_full$y >= 0,]
  
  eval_rf_full <- na.omit(eval_rf_full)
  eval_rf_full$y_hat <- log(eval_rf_full$pred_rf)
  eval_rf_full$y <- log(eval_rf_full$Flow_3t_tp1)
  eval_rf_full$year <- year
  eval_rf_full <- na.omit(eval_rf_full)
  eval_rf_full <- eval_rf_full[eval_rf_full$y_hat >= 0,]
  eval_rf_full <- eval_rf_full[eval_rf_full$y >= 0,]
  
  if(year == years[1]){
    eval_reg_full_full <- eval_reg_full
    eval_rf_full_full <- eval_rf_full
  } else {
    eval_reg_full_full <- rbind(eval_reg_full_full, eval_reg_full)
    eval_rf_full_full <- rbind(eval_rf_full_full, eval_rf_full)
  }  
}

eval_reg_simple_2009 <- eval_reg_simple_full[eval_reg_simple_full$year == 2009,]
eval_reg_simple_2010 <- eval_reg_simple_full[eval_reg_simple_full$year == 2010,]
eval_reg_simple_2011 <- eval_reg_simple_full[eval_reg_simple_full$year == 2011,]
eval_reg_simple_2012 <- eval_reg_simple_full[eval_reg_simple_full$year == 2012,]
eval_reg_simple_2013 <- eval_reg_simple_full[eval_reg_simple_full$year == 2013,]


eval_reg_bq_2009 <- eval_reg_bq_full[eval_reg_bq_full$year == 2009,]
eval_reg_bq_2010 <- eval_reg_bq_full[eval_reg_bq_full$year == 2010,]
eval_reg_bq_2011 <- eval_reg_bq_full[eval_reg_bq_full$year == 2011,]
eval_reg_bq_2012 <- eval_reg_bq_full[eval_reg_bq_full$year == 2012,]
eval_reg_bq_2013 <- eval_reg_bq_full[eval_reg_bq_full$year == 2013,]

eval_reg_un_2009 <- eval_reg_un_full[eval_reg_un_full$year == 2009,]
eval_reg_un_2010 <- eval_reg_un_full[eval_reg_un_full$year == 2010,]
eval_reg_un_2011 <- eval_reg_un_full[eval_reg_un_full$year == 2011,]
eval_reg_un_2012 <- eval_reg_un_full[eval_reg_un_full$year == 2012,]
eval_reg_un_2013 <- eval_reg_un_full[eval_reg_un_full$year == 2013,]

eval_reg_full_2009 <- eval_reg_full_full[eval_reg_full_full$year == 2009,]
eval_reg_full_2010 <- eval_reg_full_full[eval_reg_full_full$year == 2010,]
eval_reg_full_2011 <- eval_reg_full_full[eval_reg_full_full$year == 2011,]
eval_reg_full_2012 <- eval_reg_full_full[eval_reg_full_full$year == 2012,]
eval_reg_full_2013 <- eval_reg_full_full[eval_reg_full_full$year == 2013,]


eval_rf_simple_2009 <- eval_rf_simple_full[eval_rf_simple_full$year == 2009,]
eval_rf_simple_2010 <- eval_rf_simple_full[eval_rf_simple_full$year == 2010,]
eval_rf_simple_2011 <- eval_rf_simple_full[eval_rf_simple_full$year == 2011,]
eval_rf_simple_2012 <- eval_rf_simple_full[eval_rf_simple_full$year == 2012,]
eval_rf_simple_2013 <- eval_rf_simple_full[eval_rf_simple_full$year == 2013,]


eval_rf_bq_2009 <- eval_rf_bq_full[eval_rf_bq_full$year == 2009,]
eval_rf_bq_2010 <- eval_rf_bq_full[eval_rf_bq_full$year == 2010,]
eval_rf_bq_2011 <- eval_rf_bq_full[eval_rf_bq_full$year == 2011,]
eval_rf_bq_2012 <- eval_rf_bq_full[eval_rf_bq_full$year == 2012,]
eval_rf_bq_2013 <- eval_rf_bq_full[eval_rf_bq_full$year == 2013,]

eval_rf_un_2009 <- eval_rf_un_full[eval_rf_un_full$year == 2009,]
eval_rf_un_2010 <- eval_rf_un_full[eval_rf_un_full$year == 2010,]
eval_rf_un_2011 <- eval_rf_un_full[eval_rf_un_full$year == 2011,]
eval_rf_un_2012 <- eval_rf_un_full[eval_rf_un_full$year == 2012,]
eval_rf_un_2013 <- eval_rf_un_full[eval_rf_un_full$year == 2013,]


eval_rf_full_2009 <- eval_rf_full_full[eval_rf_full_full$year == 2009,]
eval_rf_full_2010 <- eval_rf_full_full[eval_rf_full_full$year == 2010,]
eval_rf_full_2011 <- eval_rf_full_full[eval_rf_full_full$year == 2011,]
eval_rf_full_2012 <- eval_rf_full_full[eval_rf_full_full$year == 2012,]
eval_rf_full_2013 <- eval_rf_full_full[eval_rf_full_full$year == 2013,]



mse_reg_simple <- MSE(eval_reg_simple_full$y_hat, eval_reg_simple_full$y)
mse_rf_simple <- MSE(eval_rf_simple_full$y_hat, eval_rf_simple_full$y)

mse_reg_bq <- MSE(eval_rf_bq_full$y_hat, eval_rf_bq_full$y)
mse_rf_bq <- MSE(eval_rf_bq_full$y_hat, eval_rf_bq_full$y)

mse_reg_un <- MSE(eval_rf_un_full$y_hat, eval_rf_un_full$y)
mse_rf_un <- MSE(eval_rf_un_full$y_hat, eval_rf_un_full$y)


importances_bq <- c(feature_importance_bq2009[2], feature_importance_bq2010[2], 
                    feature_importance_bq2011[2], feature_importance_bq2012[2],
                    feature_importance_bq2013[2])

importances_un <- c(feature_importance_un2009[2], feature_importance_un2010[2], 
                    feature_importance_un2011[2], feature_importance_un2012[2],
                    feature_importance_un2013[2])


mse_reg_simple_yearly <- c(MSE(eval_reg_simple_2009$y_hat, eval_reg_simple_2009$y),
                           MSE(eval_reg_simple_2010$y_hat, eval_reg_simple_2010$y),
                           MSE(eval_reg_simple_2011$y_hat, eval_reg_simple_2011$y),
                           MSE(eval_reg_simple_2012$y_hat, eval_reg_simple_2012$y),
                           MSE(eval_reg_simple_2013$y_hat, eval_reg_simple_2013$y))


mse_reg_bq_yearly <- c(MSE(eval_reg_bq_2009$y_hat, eval_reg_bq_2009$y),
                       MSE(eval_reg_bq_2010$y_hat, eval_reg_bq_2010$y),
                       MSE(eval_reg_bq_2011$y_hat, eval_reg_bq_2011$y),
                       MSE(eval_reg_bq_2012$y_hat, eval_reg_bq_2012$y),
                       MSE(eval_reg_bq_2013$y_hat, eval_reg_bq_2013$y))

mse_reg_un_yearly <- c(MSE(eval_reg_un_2009$y_hat, eval_reg_un_2009$y),
                       MSE(eval_reg_un_2010$y_hat, eval_reg_un_2010$y),
                       MSE(eval_reg_un_2011$y_hat, eval_reg_un_2011$y),
                       MSE(eval_reg_un_2012$y_hat, eval_reg_un_2012$y),
                       MSE(eval_reg_un_2013$y_hat, eval_reg_un_2013$y))


mse_reg_full_yearly <- c(MSE(eval_reg_full_2009$y_hat, eval_reg_full_2009$y),
                         MSE(eval_reg_full_2010$y_hat, eval_reg_full_2010$y),
                         MSE(eval_reg_full_2011$y_hat, eval_reg_full_2011$y),
                         MSE(eval_reg_full_2012$y_hat, eval_reg_full_2012$y),
                         MSE(eval_reg_full_2013$y_hat, eval_reg_full_2013$y))


mse_rf_simple_yearly <- c(MSE(eval_rf_simple_2009$y_hat, eval_rf_simple_2009$y),
                          MSE(eval_rf_simple_2010$y_hat, eval_rf_simple_2010$y),
                          MSE(eval_rf_simple_2011$y_hat, eval_rf_simple_2011$y),
                          MSE(eval_rf_simple_2012$y_hat, eval_rf_simple_2012$y),
                          MSE(eval_rf_simple_2013$y_hat, eval_rf_simple_2013$y))


mse_rf_bq_yearly <- c(MSE(eval_rf_bq_2009$y_hat, eval_rf_bq_2009$y),
                      MSE(eval_rf_bq_2010$y_hat, eval_rf_bq_2010$y),
                      MSE(eval_rf_bq_2011$y_hat, eval_rf_bq_2011$y),
                      MSE(eval_rf_bq_2012$y_hat, eval_rf_bq_2012$y),
                      MSE(eval_rf_bq_2013$y_hat, eval_rf_bq_2013$y))

mse_rf_un_yearly <- c(MSE(eval_rf_un_2009$y_hat, eval_rf_un_2009$y),
                      MSE(eval_rf_un_2010$y_hat, eval_rf_un_2010$y),
                      MSE(eval_rf_un_2011$y_hat, eval_rf_un_2011$y),
                      MSE(eval_rf_un_2012$y_hat, eval_rf_un_2012$y),
                      MSE(eval_rf_un_2013$y_hat, eval_rf_un_2013$y))

mse_rf_full_yearly <- c(MSE(eval_rf_full_2009$y_hat, eval_rf_full_2009$y),
                        MSE(eval_rf_full_2010$y_hat, eval_rf_full_2010$y),
                        MSE(eval_rf_full_2011$y_hat, eval_rf_full_2011$y),
                        MSE(eval_rf_full_2012$y_hat, eval_rf_full_2012$y),
                        MSE(eval_rf_full_2013$y_hat, eval_rf_full_2013$y))


comp <- as.data.frame(cbind(years,
                            importances_bq,
                            importances_un,
                            mse_reg_simple_yearly,
                            mse_reg_bq_yearly,
                            mse_reg_un_yearly,
                            mse_reg_full_yearly,
                            mse_rf_simple_yearly,
                            mse_rf_bq_yearly,
                            mse_rf_un_yearly,
                            mse_rf_full_yearly))

comp$years <- years+1


subtitle <- paste0("Mean squared error in predicting trade flows from third country to target during " ,sample , " sanctions")

Perf_only_sanct <- ggplot(data = comp, aes(x = years)) +
  geom_line(aes(y = mse_reg_simple_yearly, col = "regression baseline "), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_reg_bq_yearly, col ="regression with state capacity" ), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_reg_un_yearly, col ="regression with political alignment" ), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_reg_full_yearly, col ="regression full model" ), linetype = 2, size = 1.5) +
  geom_line(aes(y = mse_rf_simple_yearly, col ="random forest baseline" ), size = 1.5) +
  geom_line(aes(y = mse_rf_bq_yearly, col ="random forest with state capacity" ), size = 1.5) +
  geom_line(aes(y = mse_rf_un_yearly, col ="random forest with political alignment" ), size = 1.5) +
  geom_line(aes(y = mse_rf_full_yearly, col ="random forest full model" ), size = 1.5) +
  labs(title="Predictive performance by year predicted", 
       subtitle=subtitle,
       y="MSE",
       x="year predicted",
       color = "Model") + 
  theme(legend.position = "bottom")

graph_current <- paste0("graph_", sample)
assign(graph_current, Perf_only_sanct)
}
}
graph_UN
graph_EU
graph_US


ggsave("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_graphs/pred_UN_pot.png",
       plot = graph_UN,
       scale = 3.2,
       units = "px",
       width = 1440,
       height = 600)

ggsave("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_graphs/pred_EU_pot.png",
       plot = graph_EU,
       scale = 3.2,
       units = "px",
       width = 1440,
       height = 600)

ggsave("C:/Users/konst/Google Drive/Studium/Dissertation/paper3/_graphs/pred_US_pot.png",
       plot = graph_US,
       scale = 3.2,
       units = "px",
       width = 1440,
       height = 600)
#1440 to 600 aspect ratio
