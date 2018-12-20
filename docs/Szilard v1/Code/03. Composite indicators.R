rm(list = ls()); gc()

library(plyr)
library(dplyr)
library(pROC)
library(fastmatch)
library(classifyr)
#anne_pred <- readRDS("Z:\\SHPR\\SZILARD\\Beslutstöd\\Output\\data_with_adverse_events_2016-08-22.rds")
#source('P:/Research/People/Gudrun/Code/glmCI.R')
#source('P:/Research/People/Peter Cnudde/Trends/Code/save.xlsx.R')
#
##
###  data selection and flow chart
##       
#
## 001. OppAr < 2007
#anne_pred <- dplyr::filter(anne_pred, OppAr > 2007) 
#dim(anne_pred)
## 002. Primary arthros
#anne_pred <- dplyr::filter(anne_pred, DiaGrp == 1) 
#dim(anne_pred)
## 003. OpNr ==1 
#LPN <- dplyr::filter(anne_pred, OpNr == 2)
#anne_pred <- dplyr::filter(anne_pred, !lpnr %in% LPN$lpnr) 
#dim(anne_pred)
## 004. Age between 18 and 100
#anne_pred <- dplyr::filter(anne_pred, (17 < Alder & Alder < 101))
#dim(anne_pred)
## 005. No resurfacing 
#anne_pred <- dplyr::filter(anne_pred, ProtGrp != 5)
#dim(anne_pred)
#
##
### Calculate BMI
##
#anne_pred <- dplyr::mutate(anne_pred, bmi =  Vikt/(Langd/100)^2)
#sum(is.na(anne_pred$bmi))
#
## 006. BMI < 51 and existing
#
#anne_pred <- dplyr::filter(anne_pred, !is.na(bmi)) %>%
#             dplyr::filter( bmi  <= 50)
#dim(anne_pred)
#
## 007. ASA not 4 & 5
#
#anne_pred <- dplyr::filter(anne_pred, ASA < 4) %>%
#             droplevels()
#dim(anne_pred)
#
#
#
#
#
## 001. Gender
#
#anne_pred <- dplyr::mutate(anne_pred, Kon = factor(Kon, 
#                                   levels = c(1, 2),
#                                   labels = c('male', 'female')))  
#                                   
## 006. ASA
#anne_pred <- dplyr::mutate(anne_pred, ASA  = factor(ASA, 
#                                              levels = c(1, 2, 3),
#                                              labels = c('healthy', 'mild', 'severe')))    %>%
#             dplyr::mutate(ASA = relevel( ASA, ref = 'healthy'))
#             
#
#### -------------- Format the socioeconomic variables -----------------------###
#
## 001. Education level 
#anne_pred <- dplyr::mutate(anne_pred, Sun2000niva_old = factor(gsub(" ", "", Sun2000niva_old))) %>%
#                  dplyr::mutate(Sun2000niva_old = factor(Sun2000niva_old, 
#                                         levels = c('*', 1, 2, 3, 4, 5, 6, 7), 
#                                         labels = c('NA', 
#                                                    'pre_high_less_9yrs', 'pre_high_9yrs',
#                                                    'high_schol_2yrs', 'high_schol_3yrs', 
#                                                    'univ_less_3yrs', 'univ_3yrs',
#                                                    'graduate'),
#                                         ordered = TRUE)) 
#                                                 
#                                                    
#anne_pred$Sun2000niva_old[anne_pred$Sun2000niva_old == levels(anne_pred$Sun2000niva_old)[1]] <- NA
#anne_pred <- droplevels(anne_pred)         
#anne_pred <- dplyr::mutate(anne_pred, edu = ifelse(is.na(Sun2000niva_old), NA, 
#                                ifelse(Sun2000niva_old %in% c('pre_high_less_9yrs', 'pre_high_9yrs'),   'low',
#                                ifelse(Sun2000niva_old %in% c('high_schol_2yrs',    'high_schol_3yrs'), 'middle',
#                                                                                                        'high'))))   %>%
#                  dplyr::mutate(edu = factor(edu, 
#                                      levels = c('low', 'middle', 'high'),
#                                      labels = c('low', 'middle', 'high')))                                          %>%
#                  dplyr::mutate(edu = relevel( edu, ref = 'low'))                                                    %>%
#                  dplyr::select(-Sun2000niva_old)
#                  
#
###-------------------##
#
## 008. Not misisng Education 
#
#anne_pred <- dplyr::filter(anne_pred, !is.na(edu)) %>%
#             droplevels()
#dim(anne_pred)
#
## 002. Civil
#anne_pred <- dplyr::mutate(anne_pred, Civil = factor(gsub(" ", "", Civil))) %>%
#           dplyr::mutate(         Civil = factor(Civil, 
#                                                   levels = c('EP', 'G', 'OG', 'RP', 'S', 'SP','Ä'),
#                                                   labels = c('efterlevande_partner', 
#                                                              'gift', 
#                                                              'ogift',
#                                                              'registrerad_partner',
#                                                              'skild',
#                                                              'skild_partner',
#                                                              'änka/änkling')))
#
#
#anne_pred <- dplyr::mutate(anne_pred, civil = ifelse(is.na(Civil), NA, 
#                                ifelse(Civil %in% c('gift', 'registrerad_partner'),   'couple',
#                                ifelse(Civil %in% c('ogift',    'skild', 'skild_partner'), 'single',
#                                                                                                        'widow'))))      %>%
#                  dplyr::mutate(civil = factor(civil, 
#                                      levels = c('couple', 'single', 'widow'),
#                                      labels = c('couple', 'single', 'widow')))                                          %>%
#                  dplyr::mutate(civil = relevel( civil, ref = 'couple'))                                                 %>%
#                  dplyr::select(-Civil)             
#             
#                                                 
## 002 ATC
#
#load("P:\\Research\\People\\Anne Garland\\Mortality Perdiction\\Data\\atc_codes_AG 2016-09-14.RData")
#
#rxrisk5 <- classify(atc_codes, by = 'rxriskv_modified_icd10', id = 'lpnr', code = "atc")
#lpnrs <- rownames(rxrisk5)
#
#rxrisk5 <- apply(rxrisk5, 2, as.numeric)
#colnames(rxrisk5) <- paste('atc_', colnames(rxrisk5), '_1years', sep ='')
#
#rxrisk5 <- as.data.frame(rxrisk5)
#rxrisk5$RxR5 <- rowSums(rxrisk5)
#rxrisk5$lpnr <-lpnrs
#names(rxrisk5) <- gsub(' ', '_', names(rxrisk5))
#
#anne_pred <- merge(anne_pred, rxrisk5, by = 'lpnr')
#
#
#
#rm(list=setdiff(ls(), c('anne_pred'))); gc()

############ ------------------------------------------------------------------

load("P:\\Research\\People\\Anne Garland\\Mortality Perdiction\\Data\\anne_pred2017-02-28.RData")
anne_pred  <- dplyr::select(anne_pred, lpnr, Kon, Alder, ASA, bmi,
                                      OppDat, OppAr,  Doddat, civil, edu,  
                                      contains("elix"), contains("charlson"), contains("atc")) %>%
             dplyr::select(lpnr, Kon, Alder, ASA, bmi, 
                                      OppDat, OppAr,  Doddat, civil, edu,                                        
                                      contains("1years"))                         


##########---------------------------------------------------------------------

 anne_pred$death90day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 90))-1)
anne_pred$death365day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 365))-1)
 anne_pred$death90day[is.na( anne_pred$death90day)] <- 0
anne_pred$death365day[is.na( anne_pred$death365day)] <- 0


anne_pred <- dplyr::mutate(anne_pred,
                           heart_condition = elixhauser_congestive_heart_failure_1years + 
                                             charlson_congestive_heart_failure_1years   +
                                             atc_chronic_heart_failure_1years, 
                             heart_infarct = charlson_myocardial_infraction_1years +
                                             atc_angina_1years +
                                             atc_ischaemic_heart_disease_hypertension_1years, 
                                 arrythmia = elixhauser_cardiac_arrhythmias_1years +
                                             atc_arrhythmia_1years,
                                 hypertoni = elixhauser_hypertension_uncomplicated_1years +
                                             elixhauser_hypertension_complicated_1years +
                                             atc_hypertension_1years,
                                    kidney = elixhauser_renal_failure_1years +
                                             charlson_renal_disease_1years + 
                                             atc_end_stage_renal_disease_1years, 
                                  diabetes = elixhauser_diabetes_uncomplicated_1years + 
                                             elixhauser_diabetes_complicated_1years   +
                                             charlson_diabetes_no_complication_1years  + 
                                             charlson_diabetes_complication_1years    +
                                             atc_diabetes_1years ,
                                  vascular = elixhauser_peripheral_vascular_disorder_1years +
                                             charlson_peripheral_vascualr_disease_1years + 
                                             charlson_cerebrovascula_disease_1years +
                                             atc_anti_coagulation_therapy_1years + 
                                             atc_anti_platelet_therapy_1years)            
                                                                           


anne_pred  <- dplyr::select(anne_pred, lpnr, Kon, Alder, ASA, bmi, 
                                      death90day, death365day, 
                                      heart_condition, heart_infarct, arrythmia,
                                      hypertoni,kidney, diabetes, vascular, civil, edu) 
                                      
## - Only ASA ------------------------------------------------------------------
fit_pred_ASA <- glm(death90day ~  ASA, 
                              data = anne_pred,
                              family = 'binomial')                                                  

auc(fit_pred_ASA$model$death90day, fit_pred_ASA$fitted)
ci.auc(fit_pred_ASA$model$death90day, fit_pred_ASA$fitted) 


##                                       
fit_pred <- glm(death90day ~ Kon + Alder + ASA       +
                              I(heart_condition > 0) +
                              I(heart_infarct > 0)   +
                              I(arrythmia > 0)       +
                              I(hypertoni > 0)       +
                              I(kidney > 0)          +
                              I(diabetes > 0)        +
                              I(vascular > 0),
                              data = anne_pred,
                              family = 'binomial')
                              
summary(fit_pred)
                             
                              
fit_pred_red <- glm(death90day ~ Kon + Alder + ASA  +
                              I(heart_infarct > 0)   +
                              I(kidney > 0)          ,
                              data = anne_pred,
                              family = 'binomial')  
                              
summary(fit_pred_red)
fit_pred_plus <- glm(death90day ~ Kon + Alder + ASA  +
                              I(heart_infarct > 0)   +
                              I(kidney > 0)  + edu + civil        ,
                              data = anne_pred,
                              family = 'binomial')                                                  
                              
fit_full <- roc(fit_pred_plus$model$death90day, fit_pred_plus$fitted) 
fit_redu <- roc(fit_pred_red$model$death90day, fit_pred_red$fitted)                                                           
auc(fit_pred_red$model$death90day, fit_pred_red$fitted) 
roc.test(fit_full, fit_redu)


## -- ROC figure ----------
fit_pred_red90 <- glm(death90day ~ Kon + Alder + ASA +  
                              I(heart_infarct > 0)   +
                              I(kidney > 0)          ,
                              data = anne_pred,
                              family = 'binomial') 
                              
                               
fit_pred_red365 <- glm(death365day ~ Kon + Alder +   ASA + 
                              I(heart_infarct > 0)   +
                              I(kidney > 0)          ,
                              data = anne_pred,
                              family = 'binomial')

 r90 <- glmCI(fit_pred_red90,   exponent = TRUE, digits = 2)
r365 <- glmCI(fit_pred_red365,  exponent = TRUE, digits = 2)

save.xlsx("composite.xlsx",r90, r365)  


setwd('P:/Research/People/Anne Garland/Mortality Perdiction/Figures')
tiff(filename = "SupFig1.tif",
     width = 16,  height = 16, units= 'cm', pointsize = 10,
     compression = "lzw", bg = "white", res = 600,
     restoreConsole = TRUE, type =  "cairo")

######### ROC plot
par(mar= c(6, 5, 1, 1) + 0.1)
plot.new()
plot.window(ylim=c(0, 1), xlim=c(1, 0))
axis(1, lwd=3,cex.axis=1.5, font=2)
axis(2, lwd=3,cex.axis=1.5, font=2, las=2)
segments(x0 = 1, y0 = 0, x1 = 0, y1 = 1, col = 'grey', lwd = 2)
mtext('1-Specificity', side = 1, line = 3, cex = 2)
mtext('Sensitivity', side = 2, line = 3, cex = 2)


 fit_redu90 <- roc(fit_pred_red90$model$death90day, fit_pred_red90$fitted) 
fit_redu365 <- roc(fit_pred_red365$model$death365day, fit_pred_red365$fitted)
plot(fit_redu90, legacy.axes = TRUE, col = 'darkred',    add = TRUE, lty = 1)
plot(fit_redu365, legacy.axes = TRUE, col = 'steelblue', add = TRUE, lty = 1)

legend(x = 0.75, y = 0.15, legend = c(' 90 days: c = 0.80; 95% ci:0.76-0.84', 
                                      '365 days: c = 0.76; 95% ci:0.74-0.77'), 
                             col = c('darkred', 'steelblue'), 
                             lwd = 2,
                             lty = c(1, 1), 
                             bty = 'n')  


 dev.off()






