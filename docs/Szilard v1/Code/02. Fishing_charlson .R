rm(list = ls()); gc()

library(plyr)
library(dplyr)
library(pROC)
library(fastmatch)
anne_pred <- readRDS("Z:\\SHPR\\SZILARD\\Beslutstöd\\Output\\data_with_adverse_events_2016-08-22.rds")



#
##  data selection and flow chart
#       

# 001. OppAr < 2007
anne_pred <- dplyr::filter(anne_pred, OppAr > 2007) 
dim(anne_pred)
# 002. Primary arthros
anne_pred <- dplyr::filter(anne_pred, DiaGrp == 1) 
dim(anne_pred)
# 003. OpNr ==1 
LPN <- dplyr::filter(anne_pred, OpNr == 2)
anne_pred <- dplyr::filter(anne_pred, !lpnr %in% LPN$lpnr) 
dim(anne_pred)
# 004. Age between 18 and 100
anne_pred <- dplyr::filter(anne_pred, (17 < Alder & Alder < 101))
dim(anne_pred)
# 005. No resurfacing 
anne_pred <- dplyr::filter(anne_pred, ProtGrp != 5)
dim(anne_pred)

#
## Calculate BMI
#
anne_pred <- dplyr::mutate(anne_pred, bmi =  Vikt/(Langd/100)^2)
sum(is.na(anne_pred$bmi))

# 006. BMI < 51 and existing

anne_pred <- dplyr::filter(anne_pred, !is.na(bmi)) %>%
             dplyr::filter( bmi  <= 50)
dim(anne_pred)

# 007. ASA not 4 & 5

anne_pred <- dplyr::filter(anne_pred, ASA < 4) %>%
             droplevels()
dim(anne_pred)





### -------------- Format the socioeconomic variables -----------------------###

# 001. Education level 
anne_pred <- dplyr::mutate(anne_pred, Sun2000niva_old = factor(gsub(" ", "", Sun2000niva_old))) %>%
                  dplyr::mutate(Sun2000niva_old = factor(Sun2000niva_old, 
                                         levels = c('*', 1, 2, 3, 4, 5, 6, 7), 
                                         labels = c('NA', 
                                                    'pre_high_less_9yrs', 'pre_high_9yrs',
                                                    'high_schol_2yrs', 'high_schol_3yrs', 
                                                    'univ_less_3yrs', 'univ_3yrs',
                                                    'graduate'),
                                         ordered = TRUE)) 
                                                 
                                                    
anne_pred$Sun2000niva_old[anne_pred$Sun2000niva_old == levels(anne_pred$Sun2000niva_old)[1]] <- NA
anne_pred <- droplevels(anne_pred)         
anne_pred <- dplyr::mutate(anne_pred, edu = ifelse(is.na(Sun2000niva_old), NA, 
                                ifelse(Sun2000niva_old %in% c('pre_high_less_9yrs', 'pre_high_9yrs'),   'low',
                                ifelse(Sun2000niva_old %in% c('high_schol_2yrs',    'high_schol_3yrs'), 'middle',
                                                                                                        'high'))))   %>%
                  dplyr::mutate(edu = factor(edu, 
                                      levels = c('low', 'middle', 'high'),
                                      labels = c('low', 'middle', 'high')))                                          %>%
                  dplyr::mutate(edu = relevel( edu, ref = 'low'))                                                    %>%
                  dplyr::select(-Sun2000niva_old)
                  

##-------------------##

# 008. Not misisng Education 

anne_pred <- dplyr::filter(anne_pred, !is.na(edu)) %>%
             droplevels()
dim(anne_pred)

# 002. Civil
anne_pred <- dplyr::mutate(anne_pred, Civil = factor(gsub(" ", "", Civil))) %>%
           dplyr::mutate(         Civil = factor(Civil, 
                                                   levels = c('EP', 'G', 'OG', 'RP', 'S', 'SP','Ä'),
                                                   labels = c('efterlevande_partner', 
                                                              'gift', 
                                                              'ogift',
                                                              'registrerad_partner',
                                                              'skild',
                                                              'skild_partner',
                                                              'änka/änkling')))


anne_pred <- dplyr::mutate(anne_pred, civil = ifelse(is.na(Civil), NA, 
                                ifelse(Civil %in% c('gift', 'registrerad_partner'),   'couple',
                                ifelse(Civil %in% c('ogift',    'skild', 'skild_partner'), 'single',
                                                                                                        'widow'))))      %>%
                  dplyr::mutate(civil = factor(civil, 
                                      levels = c('couple', 'single', 'widow'),
                                      labels = c('couple', 'single', 'widow')))                                          %>%
                  dplyr::mutate(civil = relevel( civil, ref = 'couple'))                                                 %>%
                  dplyr::select(-Civil)



# 003. Clinic
anne_pred <- dplyr::mutate(anne_pred, KlinGrp  = factor(KlinGrp, 
                                    levels = c(1, 2, 3, 4),
                                    labels = c('university', 'county', 'rural', 'private')))   %>%
             dplyr::mutate(KlinGrp = relevel( KlinGrp, ref = 'university'))
             
             
# 004. ProtGrp

anne_pred <- dplyr::mutate(anne_pred, ProtGrp =  factor(ProtGrp,
                                   levels = c(1, 2, 3, 4), 
                                   labels = c('cemented', 'uncemented',
                                              'hybrid', 'reversed_hybrid')))  
                                              
# 005. Gender

anne_pred <- dplyr::mutate(anne_pred, Kon = factor(Kon, 
                                   levels = c(1, 2),
                                   labels = c('male', 'female')))  
                                   
# 006. ASA
anne_pred <- dplyr::mutate(anne_pred, ASA  = factor(ASA, 
                                              levels = c(1, 2, 3),
                                              labels = c('healthy', 'mild', 'severe')))    %>%
             dplyr::mutate(ASA = relevel( ASA, ref = 'healthy'))
                                                 


############ ------------------------------------------------------------------


anne_pred  <- dplyr::select(anne_pred, Sida, OpNr, lpnr,
                                      OppDat, OppAr,  Doddat,  
                                      contains("charlson")) %>%
             dplyr::select(Sida, OpNr, lpnr,
                                      OppDat, OppAr,  Doddat, 
                                      contains("1years"))                         


##########---------------------------------------------------------------------

 anne_pred$death90day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 90))-1)
anne_pred$death365day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 365))-1)
 anne_pred$death90day[is.na( anne_pred$death90day)] <- 0
anne_pred$death365day[is.na( anne_pred$death365day)] <- 0




res <- matrix(NA, ncol = 6, nrow = 19)
rownames(res) <- colnames(anne_pred)[7:25]
colnames(res) <- c('OR', 'LCI', 'UCI', 'C-ind', 'c_low', 'c_upp')
               
### --------------------- Survival Analysis ---------------------------------###

varlist <- names(anne_pred)[7:25]
tmpfun <- function(x) as.formula(paste("death90day", x, sep = "~"))

formlist <- lapply(varlist,tmpfun)
  models <- lapply(formlist, glm, data = anne_pred, family = 'binomial')
  
     auc <- lapply(1:19, function(x) {
    roc(models[[x]]$model$death90day, models[[x]]$fitted)$auc
})

for(i in 1:19){
res[i, 5:6] <- ci.auc(roc(models[[i]]$model$death90day, models[[i]]$fitted), 
                      conf.level = 0.95, 
                          method = c("delong"))[c(1,3)]}

options(scipen=999)

res[, 1]  <- vapply(models, function(x) exp(coef(x)[2]),    numeric(1))
res[, 2]  <- vapply(models, function(x) exp(confint(x)[2, 1]), numeric(1))
res[, 3]  <- vapply(models, function(x) exp(confint(x)[2, 2]), numeric(1))
res[, 4]  <- unlist(auc)
 res  <- round(res, 2)

setwd('P:/Research/People/Anne Garland/Mortality Perdiction/Text')                                          
source('P:/Research/People/Peter Cnudde/Trends/Code/save.xlsx.R')
save.xlsx("charlson90days_2017_02_28.xlsx",res)  

###
#miltvar_90_days <- glm(death90day ~   charlson_myocardial_infraction_1years             + charlson_congestive_heart_failure_1years    +  
#                                      charlson_peripheral_vascualr_disease_1years       + charlson_cerebrovascula_disease_1years      + 
#                                      charlson_dementia_1years                          + charlson_chronic_pulmonary_disease_1years   + 
#                                      charlson_rheumatic_disease_1years                 + charlson_peptic_ulcer_disease_1years        + 
#                                      charlson_mild_liver_disease_1years                + charlson_diabetes_no_complication_1years    + 
#                                      charlson_diabetes_complication_1years             + charlson_hemiplegia_or_paraplegia_1years    + 
#                                      charlson_renal_disease_1years                     + charlson_malingnacy_1years                  + 
#                                      charlson_moderate_or_severe_liver_disease_1years  + charlson_matastasic_solid_tumor_1years      + 
#                                      charlson_aids_hiv_1years, 
#                                      data = anne_pred, 
#                                   family = 'binomial' )
#                                   
#roc(miltvar_90_days$model$death90day, miltvar_90_days$fitted)$auc
#
#                                   
#miltvar_90_days_reduced <- step(miltvar_90_days, trace = FALSE)
#summary(miltvar_90_days_reduced)
#
#
#fit_reduced_charlson <-  glm(death90day ~   charlson_myocardial_infraction_1years             + charlson_congestive_heart_failure_1years    +  
#                                            charlson_cerebrovascula_disease_1years            + 
#                                            charlson_renal_disease_1years                     + charlson_malingnacy_1years,          
#                                         data = anne_pred, 
#                                       family = 'binomial' )
#
#
#roc(fit_reduced_charlson$model$death90day, fit_reduced_charlson$fitted)$auc
#
#
#fit_reduced_adjusted <-  glm(death90day ~   charlson_myocardial_infraction_1years             + charlson_congestive_heart_failure_1years    +  
#                                            charlson_cerebrovascula_disease_1years            + 
#                                            charlson_renal_disease_1years                     + charlson_malingnacy_1years + Alder + Kon,          
#                                         data = anne_pred, 
#                                       family = 'binomial' )
#
#
#roc_adj <- roc(fit_reduced_adjusted$model$death90day, fit_reduced_adjusted$fitted)$auc
#
#
#fit_reduced_age_sex<-  glm(death90day ~   Alder + Kon,           
#                                         data = anne_pred, 
#                                       family = 'binomial' )
#
#
#fit_reduced_age_sex_socio<-  glm(death90day ~   Alder + Kon + edu + civil + ASA,          
#                                         data = anne_pred, 
#                                       family = 'binomial' )
#roc_sex_age <- roc(fit_reduced_age_sex$model$death90day, fit_reduced_age_sex$fitted)$auc  
#roc_sex_age_socio <- roc(fit_reduced_age_sex_socio$model$death90day, fit_reduced_age_sex_socio$fitted)$auc  
#roc.test(roc_sex_age, roc_sex_age_socio)                                     
