con <- shar_linkage_con()

lisa <- tbl(con, "lisa_1yr_before") %>%
  select(LopNr, P_Side, civil_status, education)


df_shpr_orig <-
  tbl(con, "operations_factors") %>%
  left_join(tbl(con, "operations_factors_opnr")) %>%
  select(
    LopNr,
    P_Side,
    opnr,
    DateOfDeath,
    P_TypeOfHospital,
    P_ProstType,
    P_SurgDate,
    P_Gender,
    P_ASA,
    P_BMI,
    P_Age,
    P_DiaGrp,
    R_SurgDate,
    P_KVA1
  ) %>%
  filter(between(P_SurgDate, "2008-01-01", "2015-12-31")) %>%
  arrange(LopNr, P_SurgDate, R_SurgDate) %>%
  select(-R_SurgDate) %>%
  left_join(tbl(con, "elix_1yr_before")) %>%
  left_join(tbl(con, "charlson_1yr_before")) %>%
  left_join(tbl(con, "rxriskv_modified_1yr_before")) %>%
  left_join(lisa) %>%
  collect()


df_shpr_orig <-
  df_shpr_orig %>%
  mutate_at(vars(DateOfDeath, P_SurgDate), as.Date, format = "%Y-%m-%d") %>%
  mutate_at(vars(LopNr, P_ASA, P_Age), as.integer) %>%
  mutate_at(vars(P_BMI), as.numeric) %>%
  mutate_at(vars(matches("elix|charlson|rxriskv"), -contains("index")), as.logical)


cache("df_shpr_orig")
