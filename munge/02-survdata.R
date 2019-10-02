# Add important variables to data

df <-
  df_shpr %>%
  mutate_if(is.logical, coalesce, FALSE) %>%
  mutate_at(vars(contains("index")), coalesce, 0) %>%
  mutate(
    P_SurgYear = as.numeric(substr(P_SurgDate, 1, 4)),
    P_ASA = factor(P_ASA),

    # Civilstånd
    civil_status = as.character(civil_status),
    civil_status =
      ifelse(
        civil_status %in% c("divorced", "unmarried"),
        "single", civil_status) %>%
      as.factor() %>% relevel("married"),

    education =
      factor(education, levels(education), c("low", "middle", "high")),
    P_TypeOfHospital =
      factor(P_TypeOfHospital, levels(P_TypeOfHospital),
        c("University", "County", "Rural", "Private"),
      ),
    P_Sex = factor(P_Gender, c("Man", "Kvinna"), c("Male", "Female")),

    # Survival
    stime   =
      as.numeric(coalesce(DateOfDeath, as.Date("2018-02-01")) - P_SurgDate),
    status  = !is.na(DateOfDeath),

    # Outcome
    death90   = status & stime < 90,
    death365  = status & stime < 365,
    death90f  = factor(death90,  c(FALSE, TRUE), c("alive", "dead")),
    death365f = factor(death365, c(FALSE, TRUE), c("alive", "dead")),

    # truncate comorbidity indices
    charlson_icd10_index_quan_original = pmin(charlson_icd10_index_quan_original, 4),
    elix_icd10_index_sum_all           = pmin(elix_icd10_index_sum_all, 3),
  ) %>%

  # Remove variables not needed for analysis
  select(
    -P_AcetCupCemMix,
    -P_FemStemCemMix,
    -LopNr,
    -P_Side,
    -opnr,
    -DateOfDeath,
    -P_SurgDate,
    -P_DiaGrp,
    -P_KVA1,
    -P_ProstType,
    -contains("index"),
    elix_icd10_index_sum_all,
    charlson_icd10_index_quan_original
  ) %>%
  # Shorter names
  rename_all(~ gsub("charlson_icd10", "CCI", .)) %>%
  rename_all(~ gsub("elix_icd10", "ECI", .))

