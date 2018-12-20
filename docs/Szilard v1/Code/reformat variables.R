
#


anne_pred <- dplyr::select(anne_pred, lpnr, OpNr, 
                                    OppDat, OppAr, OprAr, Doddat, 
                                    DatOpr,                                     # Reoperation  
                                    OprDat,                                     # Revision  
                                    KlinGrp,
                                    ProtGrp, Snitt, 
                                    eq10, eq20, eq30, eq40, eq50, halsvas0, smrtvas0,   
                                    Kon, Alder, 
                                    DiaGrp, 
                                    charkat0, ASA, Langd, Vikt, 
                                    elix_index_1years, atc_index_modified_1year,
                                    Sun2000niva_old, Civil)
                                    
                                    
#
## 01. Only the first operation  and primary atrthrosis
#
anne_pred <- dplyr::filter(anne_pred, OpNr == 1, DiaGrp ==1) %>%
          dplyr::select(-OpNr, -DiaGrp)

#
##
#

anne_pred <- dplyr::mutate(anne_pred, charkat0 =  as.factor(charkat0))                                                                                                                                          
anne_pred$charkat0[anne_pred$charkat0 == levels(anne_pred$charkat0)[1]] <- NA  
anne_pred <- droplevels(anne_pred) 

#
## 03. Reformat education 
#

# Education level 
anne_pred <- dplyr::mutate(anne_pred, Sun2000niva_old = factor(gsub(" ", "", Sun2000niva_old))) %>%
                  dplyr::mutate(Sun2000niva_old = factor(Sun2000niva_old, 
                                         levels = c('*', 1, 2, 3, 4, 5, 6, 7), 
                                         labels = c('NA', 
                                                    'pre_high_less_9yrs', 'pre_high_9yrs',
                                                    'high_schol_2yrs', 'high_schol_3yrs', 
                                                    'univ_less_3yrs', 'univ_3yrs',
                                                    'graduate'),
                                         ordered = TRUE)) 
                                                 
                                                    

         
anne_pred <- dplyr::mutate(anne_pred, edu = ifelse(is.na(Sun2000niva_old), NA, 
                                ifelse(Sun2000niva_old %in% c('pre_high_less_9yrs', 'pre_high_9yrs'),   'low',
                                ifelse(Sun2000niva_old %in% c('high_schol_2yrs',    'high_schol_3yrs'), 'middle',
                                                                                                        'high'))))   %>%
                  dplyr::mutate(edu = factor(edu, 
                                      levels = c('low', 'middle', 'high'),
                                      labels = c('low', 'middle', 'high')))                                          %>%
                  dplyr::mutate(edu = relevel( edu, ref = 'low'))                                                    %>%
                  dplyr::select(-Sun2000niva_old)
                  
#
## 04. Civil
#
 
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

#
## 05. Clinic
#
anne_pred <- dplyr::mutate(anne_pred, KlinGrp  = factor(KlinGrp, 
                                    levels = c(1, 2, 3, 4),
                                    labels = c('university', 'county', 'rural', 'private')))   %>%
             dplyr::mutate(KlinGrp = relevel( KlinGrp, ref = 'university'))
             
#             
## 06. ProtGrp
#
anne_pred <- dplyr::mutate(anne_pred, ProtGrp =  factor(ProtGrp,
                                   levels = c(1, 2, 3, 4), 
                                   labels = c('cemented', 'uncemented',
                                              'hybrid', 'reversed_hybrid')))  
                                              

                                                              
                                                                                                                