###
#
#  90 day mortality perdiction 
#
#   Anne 2016-09-13
#
###
library(dplyr)
library(caret)
library(pROC)
library(binom)
library(survival)
## Read in the data file 
load("P:\\Research\\People\\Anne Garland\\Mortality Perdiction\\Data\\anne_pred2016-10-25.RData")
source('P:/Research/People/Peter Cnudde/Trends/Code/save.xlsx.R')
source('P:/Research/People/Gudrun/Code/glmCI.R')



### calculate the survival time ------------------------------------------------
anne_pred$death <- factor(is.na(anne_pred$Doddat),
                   levels = c('FALSE', 'TRUE'),
                   labels = c('Dead', 'Alive'))
                   
                   
# for missing Death date censoring time is set at 2012-12-31
anne_pred$Doddat2 <- anne_pred$Doddat
anne_pred$Doddat2[is.na(anne_pred$Doddat2)] <- '2012-12-31'
anne_pred$Survtime <- with(anne_pred, as.numeric(Doddat2 - OppDat))
anne_pred$Doddat2 <- NULL  

### Survival rates

survEst <- function(fit, times = NULL){
   require(survival)
   
  if(class(fit)!="survfit")
       return('Error! surfit object is needed')
          else
   if(is.null(times))
        return('Please define the time points')
          else
  if(is.null(fit$strata))

res <- as.data.frame( summary(fit, times = times )[c("time", "surv",  "lower", "upper")]) 
           else
res <- as.data.frame( summary(fit, times = times )[c("strata", "time","surv", "lower", "upper")])       

    return(res)
}

fit_raw <- survfit(formula = Surv(Survtime, death =='Dead') ~ 1, data = anne_pred)

# a KM plot
plot(fit_raw)

# Survival prob at 2 years
survEst(fit_raw, times = c(90, 365, 5*365))



## 90 day mortality outcome variable


                 
 anne_pred$death90day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 90))-1)
anne_pred$death365day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 365))-1)
 anne_pred$death90day[is.na( anne_pred$death90day)] <- 0
anne_pred$death365day[is.na( anne_pred$death365day)] <- 0


## Co-morb index with NA values set to zero 
anne_pred$elix_index_1years[is.na( anne_pred$elix_index_1years)] <- 0
anne_pred$charlson_index_original_1years[is.na( anne_pred$charlson_index_original_1years)] <- 0
anne_pred$atc_index_modified_1year[is.na( anne_pred$atc_index_modified_1year)] <- 0



#### Prediction modeling for the 90 day mortality  -----------------------------

fit90days <- glm(death90day ~ Alder + Kon + bmi + 
                              KlinGrp + 
                              ProtGrp + 
                              ASA + elix_index_1years + charlson_index_original_1years + atc_index_modified_1year + 
                              edu + civil, data = anne_pred, family = 'binomial')
                              
resMulti <- glmCI(fit90days, exponent = TRUE, digits = 2)                              
setwd('P:/Research/People/Anne Garland/Mortality Perdiction/Text')                                          

save.xlsx("resMulti.xlsx",resMulti)                               


### Univariate Ressults
     fit_age <- glm(death90day ~ Alder,                          data = anne_pred, family = 'binomial')
     fit_bmi <- glm(death90day ~ bmi,                            data = anne_pred, family = 'binomial')
  fit_gender <- glm(death90day ~ Kon,                            data = anne_pred, family = 'binomial')
  fit_klinik <- glm(death90day ~ KlinGrp,                        data = anne_pred, family = 'binomial')
  fit_protes <- glm(death90day ~ ProtGrp,                        data = anne_pred, family = 'binomial')
     fit_asa <- glm(death90day ~ ASA,                            data = anne_pred, family = 'binomial')
    fit_elix <- glm(death90day ~ elix_index_1years,              data = anne_pred, family = 'binomial')
fit_charlson <- glm(death90day ~ charlson_index_original_1years, data = anne_pred, family = 'binomial')
     fit_atc <- glm(death90day ~ atc_index_modified_1year,       data = anne_pred, family = 'binomial')
     fit_edu <- glm(death90day ~ edu,                            data = anne_pred, family = 'binomial') 
   fit_civil <- glm(death90day ~ civil,                          data = anne_pred, family = 'binomial')   

fit <- fit_civil  
auc(fit$model$death90day, fit$fitted)   

resUnivarait <- rbind(glmCI(fit_age,      exponent = TRUE, digits = 2),  
                      glmCI(fit_gender,   exponent = TRUE, digits = 2),
                      glmCI(fit_klinik,   exponent = TRUE, digits = 2),
                      glmCI(fit_protes,   exponent = TRUE, digits = 2),
                      glmCI(fit_asa,      exponent = TRUE, digits = 2),  
                      glmCI(fit_elix,     exponent = TRUE, digits = 2),
                      glmCI(fit_charlson, exponent = TRUE, digits = 2),
                      glmCI(fit_atc,      exponent = TRUE, digits = 2),
                      glmCI(fit_edu,      exponent = TRUE, digits = 2),
                      glmCI(fit_civil,    exponent = TRUE, digits = 2))
 
write.table(resUnivarait, 'resUnivarait.txt', sep = '\t')     
  
  
  
  
  
###-----------------------------------------------------------------------------
fit90days_reduced <- step(fit90days, trace = FALSE) 

fit90days_reduced  <- glm(death90day ~ Alder + Kon +                             
                              ASA +  charlson_index_original_1years + civil, data = anne_pred, family = 'binomial')


## Model validation 
 anne_pred$death90factor <- factor(anne_pred$death90day,
                        levels = c('0', '1'),
                        labels = c('Alive', 'Dead'))
                        
                        
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                    repeats = 5,
            summaryFunction = twoClassSummary,
                 classProbs = TRUE,
            savePredictions = TRUE)


##  Predictive model training
lrFit <- train(death90factor ~  Alder + Kon +                             
                              ASA +  charlson_index_original_1years + civil,
                  data = anne_pred,
                method = "glm",
                metric = "ROC",
             trControl = ctrl)

lrFit


## Get the area under the ROC curve for the hold-out set
lrRoc <- roc(response = lrFit$pred$obs,
            predictor = lrFit$pred$Alive,
               levels = rev(levels(lrFit$pred$obs)))
plot(lrRoc, legacy.axes = TRUE)
auc(lrRoc); ci.auc(lrRoc)

## variable importance
lrImp <- varImp(lrFit, scale = FALSE)
lrImp




#### Prediction modeling for the 365day mortality  -----------------------------

fit365days <- glm(death365day ~ Alder + Kon + bmi + 
                              KlinGrp + 
                              ProtGrp + 
                              ASA + elix_index_1years + charlson_index_original_1years + atc_index_modified_1year + 
                              edu + civil, data = anne_pred, family = 'binomial')


fit365days_reduced <- step(fit365days, trace = FALSE) 

fit90days_reduced  <- glm(death365day ~ Alder + Kon + bmi+                            
                              ASA +  charlson_index_original_1years + civil, data = anne_pred, family = 'binomial')


## Model validation 
 anne_pred$death365factor <- factor(anne_pred$death365day,
                        levels = c('0', '1'),
                        labels = c('Alive', 'Dead'))
                        
                        
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                    repeats = 5,
            summaryFunction = twoClassSummary,
                 classProbs = TRUE,
            savePredictions = TRUE)


##  Predictive model training
lrFit <- train(death365factor ~  Alder + Kon + bmi+                            
                              ASA +  charlson_index_original_1years + civil,
                  data = anne_pred,
                method = "glm",
                metric = "ROC",
             trControl = ctrl)

lrFit


## Get the area under the ROC curve for the hold-out set
lrRoc <- roc(response = lrFit$pred$obs,
            predictor = lrFit$pred$Alive,
               levels = rev(levels(lrFit$pred$obs)))
plot(lrRoc, legacy.axes = TRUE)
auc(lrRoc); ci.auc(lrRoc)

## variable importance
lrImp <- varImp(lrFit, scale = FALSE)
lrImp


################

##
fit90daysASA <- glm(death90day ~   ASA +
                              Alder + Kon + bmi + 
                              KlinGrp + 
                              ProtGrp + 
                              edu + civil, data = anne_pred, family = 'binomial')
                              
glmCI(fit90daysASA, exponent = TRUE, digits = 2) 

##
fit90daysElix <- glm(death90day ~   elix_index_1years +
                              Alder + Kon + bmi + 
                              KlinGrp + 
                              ProtGrp + 
                              edu + civil, data = anne_pred, family = 'binomial')
                              
glmCI(fit90daysElix, exponent = TRUE, digits = 2)

##
fit90daysCharls <- glm(death90day ~   charlson_index_original_1years +
                              Alder + Kon + bmi + 
                              KlinGrp + 
                              ProtGrp + 
                              edu + civil, data = anne_pred, family = 'binomial')
                              
glmCI(fit90daysCharls, exponent = TRUE, digits = 2) 
##
fit90daysATC <- glm(death90day ~   atc_index_modified_1year +
                              Alder + Kon + bmi + 
                              KlinGrp + 
                              ProtGrp + 
                              edu + civil, data = anne_pred, family = 'binomial')
                              
glmCI(fit90daysATC, exponent = TRUE, digits = 2)  
