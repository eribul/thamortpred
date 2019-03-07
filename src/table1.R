library(ProjectTemplate)
load.project()

fctrs <- c(
  "P_ASA",
  "ECI_index_walraven",
  "CCI_index_quan_original",
  "Rx_index_index"
)

t1 <-
  tableone::CreateTableOne(
    vars = unique(c(fctrs, predictors)),
    factorVars = fctrs,
    data = df
  )

cache("t1")

table1 <-
  t1 %>%
  print(
    showAllLevels = TRUE,
    printToggle = FALSE
  ) %>%
  as_tibble(rownames = "what")

writexl::write_xlsx(table1, "reports/table1.xlsx")

