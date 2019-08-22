lisa <- tbl(con, "lisa_1yr_before") %>%
  select(LopNr, P_Side, civil_status, education)

df_shpr_orig <-
  tbl(con, "primary") %>%
  select(
    LopNr,
    P_Side,
    DateOfDeath,
    P_TypeOfHospital,
    P_ProstType,
    P_SurgDate,
    P_Gender,
    P_ASA,
    P_BMI,
    P_Age,
    P_DiaGrp,
    P_KVA1,
    P_AcetCupCemMix,
    P_FemStemCemMix
  ) %>%
  filter(between(P_SurgDate, "2008-01-01", "2015-12-31")) %>%
  left_join(tbl(con, "operations_factors_opnr"), c("LopNr", "P_Side")) %>%
  left_join(tbl(con, "elix_1yr_before"), c("LopNr", "P_Side")) %>%
  left_join(tbl(con, "charlson_1yr_before"), c("LopNr", "P_Side")) %>%
  left_join(lisa, c("LopNr", "P_Side")) %>%
  collect() %>%

  mutate_if(is.character, na_if, "NA") %>%
  mutate_at(vars(DateOfDeath, P_SurgDate), as.Date, format = "%Y-%m-%d") %>%
  mutate_at(vars(LopNr, P_ASA, P_Age), as.integer) %>%
  mutate_at(vars(P_BMI), as.numeric) %>%
  mutate_at(vars(matches("elix|charlson"), -contains("index")), as.logical) %>%
  mutate_if(is.character, as.factor)


cache("df_shpr_orig")
