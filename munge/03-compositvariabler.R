# Förbered kompisitvariabler som ev kan ersätta enskilda diagnoser

df <-
  df %>%
  mutate(

    c_heart_condition =
      ECI_congestive_heart_failure              |
      CCI_congestive_heart_failure              |
      ECI_valvular_disease,
    c_heart_infarct =
      CCI_myocardial_infarction,
    c_arrythmia =
      ECI_cardiac_arrhythmias,
    c_hypertoni =
      ECI_hypertension_uncomplicated            |
      ECI_hypertension_complicated,
    c_kidney_disease =
      ECI_renal_failure                         |
      CCI_renal_disease,
    c_diabetes =
      ECI_diabetes_uncomplicated                |
      ECI_diabetes_complicated                  |
      CCI_diabetes_without_complication         |
      CCI_diabetes_complication,
    c_vascular_disease =
      ECI_peripheral_vascular_disorder          |
      CCI_peripheral_vascular_disease           |
      CCI_cerebrovascular_disease,
    c_cancer =
      ECI_lymphoma                              |
      ECI_metastatic_cancer                     |
      ECI_solid_tumor                           |
      CCI_malingnancy                           |
      CCI_metastatic_solid_tumor,
    c_aids_hiv =
      ECI_aids_hiv                              |
      CCI_aids_hiv,
    c_liver_disease =
      ECI_liver_disease                         |
      CCI_mild_liver_disease                    |
      CCI_moderate_or_severe_liver_disease,
    c_lung_airways_disease =
      ECI_chronic_pulmonary_disease             |
      ECI_pulmonary_circulation_disorder        |
      CCI_chronic_pulmonary_disease,
    c_drug_alcohol_abuse =
      ECI_alcohol_abuse                         |
      ECI_drug_abuse,
    c_cns =
      CCI_dementia                              |
      ECI_depression                            |
      ECI_paralysis                             |
      CCI_hemiplegia_or_paraplegia              |
      ECI_other_neurological_disorders          |
      ECI_psychoses,
    c_reuma =
      ECI_rheumatoid_arthritis                  |
      CCI_rheumatic_disease,
    c_anemia =
      ECI_blood_loss_anemia                     |
      ECI_deficiency_anemia,
    c_peptic_ulcer =
      ECI_peptic_ulcer_disease                  |
      CCI_peptic_ulcer_disease
  ) %>%

  select(
    everything(),
    -matches("^([CE]CI)|(Rx)_"),

    ECI_hypothyroidism,
    ECI_coagulopathy,
    ECI_obesity,
    ECI_weight_loss,
    ECI_fluid_electrolyte_disorders,

    matches("index")
  )

cache("df")
