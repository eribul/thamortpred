rm(list = ls()); gc()

library(plyr)
library(dplyr)
library(pROC)
library(fastmatch)
library(classifyr)
anne_pred <- readRDS("Z:\\SHPR\\SZILARD\\Beslutstöd\\Output\\data_with_adverse_events_2016-08-22.rds")
source('P:/Research/People/Gudrun/Code/glmCI.R')
source('P:/Research/People/Peter Cnudde/Trends/Code/save.xlsx.R')

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





# 001. Gender

anne_pred <- dplyr::mutate(anne_pred, Kon = factor(Kon, 
                                   levels = c(1, 2),
                                   labels = c('male', 'female')))  
                                   
# 006. ASA
anne_pred <- dplyr::mutate(anne_pred, ASA  = factor(ASA, 
                                              levels = c(1, 2, 3),
                                              labels = c('healthy', 'mild', 'severe')))    %>%
             dplyr::mutate(ASA = relevel( ASA, ref = 'healthy'))
             

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
             
                                                 
# 002 ATC

load("P:\\Research\\People\\Anne Garland\\Mortality Perdiction\\Data\\atc_codes_AG 2016-09-14.RData")

rxrisk5 <- classify(atc_codes, by = 'rxriskv_modified_icd10', id = 'lpnr', code = "atc")
lpnrs <- rownames(rxrisk5)

rxrisk5 <- apply(rxrisk5, 2, as.numeric)
colnames(rxrisk5) <- paste('atc_', colnames(rxrisk5), '_1years', sep ='')

rxrisk5 <- as.data.frame(rxrisk5)
rxrisk5$RxR5 <- rowSums(rxrisk5)
rxrisk5$lpnr <-lpnrs


anne_pred <- merge(anne_pred, rxrisk5, by = 'lpnr')





############ ------------------------------------------------------------------


anne_pred  <- dplyr::select(anne_pred, lpnr, Kon, Alder, ASA, bmi,
                                      OppDat, OppAr,  Doddat, civil, edu,  
                                      contains("elix"), contains("charlson"), contains("atc"), atc_index_modified_1year) %>%
             dplyr::select(lpnr, Kon, Alder, ASA, bmi, 
                                      OppDat, OppAr,  Doddat, civil, edu,                                        
                                      contains("1years"), atc_index_modified_1year)                         


##########---------------------------------------------------------------------

 anne_pred$death90day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 90))-1)
anne_pred$death365day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 365))-1)
 anne_pred$death90day[is.na( anne_pred$death90day)] <- 0
anne_pred$death365day[is.na( anne_pred$death365day)] <- 0


                              

## -- ROC figure ----------
 fit_ASA <- glm(death90day ~ ASA, data = anne_pred, family = 'binomial') 
fit_elix <- glm(death90day ~ elix_index_1years, data = anne_pred, family = 'binomial') 
fit_char <- glm(death90day ~ charlson_index_original_1years, data = anne_pred, family = 'binomial')                               
 fit_atc <- glm(death90day ~ atc_index_modified_1year, data = anne_pred, family = 'binomial') 
fit_finall <- glm(death90day ~ ASA + Kon + Alder, data = anne_pred, family = 'binomial') 
setwd('P:/Research/People/Anne Garland/Mortality Perdiction/Figures')
tiff(filename = "AUC_IndexesNov25.tif",
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
 
## ASA
 auc_ASA <- roc(fit_ASA$model$death90day, fit_ASA$fitted) 
plot(auc_ASA, legacy.axes = TRUE, col = 'darkred',    add = TRUE, lty = 1)

## Elixhauser
auc_Elix <- roc(fit_elix$model$death90day, fit_elix$fitted)
plot(auc_Elix, legacy.axes = TRUE, col = 'steelblue', add = TRUE, lty = 1)

## Charlson
auc_Char <- roc(fit_char$model$death90day, fit_char$fitted)
plot(auc_Char, legacy.axes = TRUE, col = 'black', add = TRUE, lty = 1)

## ATC
auc_ATC <- roc(fit_atc$model$death90day, fit_atc$fitted)
plot(auc_ATC, legacy.axes = TRUE, col = 'green', add = TRUE, lty = 1)

## Finall
auc_Fin <- roc(fit_finall$model$death90day, fit_finall$fitted)
plot(auc_Fin, legacy.axes = TRUE, col = 'Orange', add = TRUE, lty = 1)

legend(x = 0.60, y = 0.20, legend = c('ASA: c = 0.67; 95% ci:0.66-0.74',
                                      'Elixhauser: c =  0.62; 95% ci:0.57-0.68', 
                                      'Charlson: c =  0.64; 95% ci:0.60-0.70',
                                      'RxRisk V: c = 0.66; 95% ci:0.61-0.71',
                                      'New Model: c = 0.80; 95% ci:0.76-0.84'), 
                             col = c('darkred', 'steelblue', 'black', 'green', 'orange'), 
                             lwd = 2,
                             bty = 'n')  


 dev.off()






