# Jag vill med detta skript återskapa tidigare flödesschema från Szilard med gamla data
# för att kolla så vi gör motsvarande urval etc.

library(ProjectTemplate)
load.project()

anne_pred_orig <- readRDS(
  "//RCHOST9/Stat_Arkiv/Z_Arkiv/SHPR/Szilard Nemes/Beslutstöd/Output/data_with_adverse_events_2016-08-22.rds")
ane_pred <- anne_pred_orig


# 001. OppAr < 2007
anne_pred <- dplyr::filter(anne_pred, OppAr > 2007)
dim(anne_pred)

# 002. Primary arthros
anne_pred <- dplyr::filter(anne_pred, DiaGrp == 1)
dim(anne_pred)

# 003. Only pateinst with 1 oprration
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
                                                   levels = c('EP', 'G', 'OG', 'RP', 'S', 'SP','Ã„'),
                                                   labels = c('efterlevande_partner',
                                                              'gift',
                                                              'ogift',
                                                              'registrerad_partner',
                                                              'skild',
                                                              'skild_partner',
                                                              'Ã¤nka/Ã¤nkling')))


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
                                              'hybrid', 'reversed_hybrid')))                   %>%
             dplyr::mutate(ProtGrp = relevel( ProtGrp, ref = 'cemented'))

# 005. Gender

anne_pred <- dplyr::mutate(anne_pred, Kon = factor(Kon,
                                   levels = c(1, 2),
                                   labels = c('male', 'female')))

# 006. ASA
anne_pred <- dplyr::mutate(anne_pred, ASA  = factor(ASA,
                                              levels = c(1, 2, 3),
                                              labels = c('healthy', 'mild', 'severe')))    %>%
             dplyr::mutate(ASA = relevel( ASA, ref = 'healthy'))


anne_pred$charkat0 <- as.factor(anne_pred$charkat0)
anne_pred$charkat0[anne_pred$charkat0 == levels(anne_pred$charkat0)[1]] <- NA

##########---------------------------------------------------------------------

 anne_pred$death90day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 90))-1)
anne_pred$death365day <- with(anne_pred, as.numeric(factor(as.numeric(Doddat - OppDat) <= 365))-1)
 anne_pred$death90day[is.na( anne_pred$death90day)] <- 0
anne_pred$death365day[is.na( anne_pred$death365day)] <- 0




### Table 1 --------------------------------------------------------------------

data_table1 <- dplyr::select(anne_pred, Kon, Alder, bmi,
                                        edu, civil,
                                        ASA, elix_index_1years,  charlson_index_original_1years, atc_index_modified_1year,
                                        KlinGrp, ProtGrp)
data_table1$elix <- data_table1$elix_index_1years
data_table1$elix[data_table1$elix > 3] <- 4

data_table1$charlson <- data_table1$charlson_index_original_1years
data_table1$charlson[data_table1$charlson > 3] <- 4

data_table1$rxrisk5 <- data_table1$atc_index_modified_1year
data_table1$rxrisk5[data_table1$rxrisk5 > 6] <- 7

dim(anne_pred)


tableOne <- CreateTableOne(vars = colnames(data_table1),  factorVars = c('elix', 'charlson','rxrisk5'), data = data_table1, includeNA = TRUE)
t1 <- print(tableOne)
setwd('P:/Research/People/Anne Garland/Mortality Perdiction/Text')
write.table(t1, file="t1.txt", sep = '\t')

rm(list=setdiff(ls(), c("anne_pred")))
