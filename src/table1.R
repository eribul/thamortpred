library(ProjectTemplate)
load.project()

t1 <-
  tableone::CreateTableOne(
    vars = predictors,
    factorVars = c(
      "P_ASA",
      "ECI_index_walraven",
      "CCI_index_quan_original",
      "Rx_index_index"
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

