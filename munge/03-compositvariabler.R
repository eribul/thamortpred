# Förbered kompisitvariabler som ev kan ersätta enskilda diagnoser

df <-
  df %>%
  mutate(

    c_heart_condition =
      ECI_congestive_heart_failure              |
      CCI_congestive_heart_failure              |
      Rx_chronic_heart_failure                  |
      ECI_valvular_disease,
    c_heart_infarct =
      CCI_myocardial_infarction                 |
      Rx_angina                                 |
      Rx_ischaemic_heart_disease_hypertension,
    c_arrythmia =
      ECI_cardiac_arrhythmias                   |
      Rx_arrhythmia,
    c_hypertoni =
      ECI_hypertension_uncomplicated            |
      ECI_hypertension_complicated              |
      Rx_hypertension,
    c_kidney =
      ECI_renal_failure                         |
      CCI_renal_disease                         |
      Rx_end_stage_renal_disease,
    c_diabetes =
      ECI_diabetes_uncomplicated                |
      ECI_diabetes_complicated                  |
      CCI_diabetes_without_complication         |
      CCI_diabetes_complication                 |
      Rx_diabetes,
    c_vascular =
      ECI_peripheral_vascular_disorder          |
      CCI_peripheral_vascular_disease           |
      CCI_cerebrovascular_disease               |
      Rx_anti_coagulation_therapy               |
      Rx_anti_platelet_therapy,
    c_cancer =
      ECI_lymphoma                              |
      ECI_metastatic_cancer                     |
      ECI_solid_tumor                           |
      CCI_malingnancy                           |
      CCI_metastatic_solid_tumor                |
      Rx_malignancies,
    c_aids_hiv =
      ECI_aids_hiv                              |
      CCI_aids_hiv                              |
      Rx_hiv,
    c_liver_disease =
      ECI_liver_disease                         |
      CCI_mild_liver_disease                    |
      CCI_moderate_or_severe_liver_disease      |
      Rx_liver_failure                          |
      Rx_hepatitis_c,
    c_lung_airways =
      ECI_chronic_pulmonary_disease             |
      ECI_pulmonary_circulation_disorder        |
      CCI_chronic_pulmonary_disease             |
      Rx_smoking_cessation                      |
      Rx_chronic_airways_disease                |
      Rx_tuberculosis,
    c_drug_alcohol_abuse =
      ECI_alcohol_abuse                         |
      ECI_drug_abuse                            |
      Rx_alcohol_dependence,
    c_cns =
      CCI_dementia                              |
      Rx_dementia                               |
      ECI_depression                            |
      Rx_depression                             |
      ECI_paralysis                             |
      CCI_hemiplegia_or_paraplegia              |
      ECI_other_neurological_disorders          |
      ECI_psychoses                             |
      Rx_anxiety                                |
      Rx_bipolar_disorder                       |
      Rx_epilepsy                               |
      Rx_migraine                               |
      Rx_parkinsons_disease                     |
      Rx_psychotic_illness,
    c_reuma =
      ECI_rheumatoid_arthritis                  |
      CCI_rheumatic_disease,
    c_anemia =
      ECI_blood_loss_anemia                     |
      ECI_deficiency_anemia,
    c_peptic_ulcer =
      ECI_peptic_ulcer_disease                  |
      CCI_peptic_ulcer_disease                  |
      Rx_gastric_oesophageal_reflux_disorder    |
      Rx_inflammatory_bowel_disease,

  ) %>%

  select(
    everything(),
    -matches("^([CE]CI)|(Rx)_"),

    ECI_hypothyroidism,
    ECI_coagulopathy,
    ECI_obesity,
    ECI_weight_loss,
    ECI_fluid_electrolyte_disorders,

    Rx_allergies,
    Rx_benign_prostate_hypertrophy,
    Rx_glaucoma,
    Rx_gout,
    Rx_hyperkalaemia,
    Rx_hyperlipidemia,
    Rx_hyperthyroidism,
    Rx_malnutrition,
    Rx_osteoporosis_pagets,
    Rx_pain,
    Rx_pancreatic_insufficiency,
    Rx_psoriasis,
    Rx_steroid_responsive_diseases,
    Rx_transplant,

    matches("index")
  )

cache("df")
