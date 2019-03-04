# Förbered kompisitvariabler som ev kan ersätta enskilda diagnoser

df <-
  df %>%
  mutate(
    c_heart_condition =
      ECI_congestive_heart_failure +
      CCI_congestive_heart_failure +
      Rx_chronic_heart_failure,
    c_heart_infarct =
      CCI_myocardial_infarction +
      Rx_angina +
      Rx_ischaemic_heart_disease_hypertension,
    c_arrythmia =
      ECI_cardiac_arrhythmias +
      Rx_arrhythmia,
    c_hypertoni =
      ECI_hypertension_uncomplicated +
      ECI_hypertension_complicated +
      Rx_hypertension,
    c_kidney =
      ECI_renal_failure +
      CCI_renal_disease +
      Rx_end_stage_renal_disease,
    c_diabetes =
      ECI_diabetes_uncomplicated +
      ECI_diabetes_complicated +
      CCI_diabetes_without_complication +
      CCI_diabetes_complication +
      Rx_diabetes,
    c_vascular =
      ECI_peripheral_vascular_disorder +
      CCI_peripheral_vascular_disease +
      CCI_cerebrovascular_disease +
      Rx_anti_coagulation_therapy +
      Rx_anti_platelet_therapy
  ) %>%
  mutate_at(vars(starts_with("c_")), ~. > 0)

cache("df")
