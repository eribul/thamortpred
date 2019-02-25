library(ProjectTemplate)
load.project()

t1 <-
  tableone::CreateTableOne(
    vars = predictors,
    factorVars = c(
      "P_ASA",
      "elix_icd10_index_walraven",
      "charlson_icd10_index_quan_original",
      "rxriskv_modified_atc_index_index"
    ),
    data = df
  )

cache("table1")

table1 <-
  t1 %>%
  print(
    showAllLevels = TRUE,
    printToggle = FALSE
  ) %>%
  as_tibble(rownames = "what")

writexl::write_xlsx(table1, "reports/table1.xlsx")

