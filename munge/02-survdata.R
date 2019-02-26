# Add important variables to data

df <-
  df_shpr %>%
  mutate_if(
    is.logical,
    coalesce, FALSE
  ) %>%
  mutate_at(
    vars(contains("index")),
    ~ifelse(is.na(.), 0, .)
  ) %>%
  mutate(
    P_SurgYear = as.factor(substr(P_SurgDate, 1, 4)),
    P_ASA = as.factor(P_ASA),

    # Protestyp
    acetcem = P_AcetCupCemMix != "Cementfritt",
    stemcem = P_FemStemCemMix != "Cementfritt",
    P_ProtGrp = case_when(
      !acetcem & !stemcem ~ 'Cementless',
      !acetcem &  stemcem ~ 'Hybrid',
      acetcem  & !stemcem ~ 'Reversed hybrid',
      acetcem  &  stemcem ~ 'Cemented'
    ) %>% as.factor() %>% relevel("Cemented"),

    # Civilstånd
    civil_status = as.character(civil_status),
    civil_status =
      ifelse(civil_status %in% c("divorced", "unmarried"), "single", civil_status) %>%
      as.factor() %>% relevel("married"),

    education = relevel(education, "low"),
    P_TypeOfHospital = relevel(P_TypeOfHospital, "Universitets- eller regionssjukhus"),
    P_Gender = relevel(P_Gender, "Man"),

    # Survival
    stime   = as.numeric(coalesce(DateOfDeath, as.Date("2018-02-01")) - P_SurgDate),
    status  = !is.na(DateOfDeath),

    # Outcome
    death90    = status & stime < 90,
    death365   = status & stime < 365,
    death90f  = factor(death90,  c(FALSE, TRUE), c("alive", "dead")),
    death365f = factor(death365, c(FALSE, TRUE), c("alive", "dead"))
  ) %>%

  # Remove variables not needed for analysis
  select(
    -P_AcetCupCemMix,
    -P_FemStemCemMix,
    -acetcem,
    -stemcem,
    -LopNr,
    -P_Side,
    -opnr,
    -DateOfDeath,
    -P_SurgDate,
    -P_DiaGrp,
    -P_KVA1,
    -time_between,
    -new,
    -bilateral,
    -P_ProstType,
    -contains("index"),
    rxriskv_modified_atc_index_index,
    elix_icd10_index_walraven,
    # charlson_icd10_index_quan_updated,
    charlson_icd10_index_quan_original
  ) %>%
  rename_all(~ gsub("charlson_icd10", "CCI", .)) %>%
  rename_all(~ gsub("rxriskv_modified_atc", "Rx", .)) %>%
  rename_all(~ gsub("elix_icd10", "ECI", .))


cache("df")
