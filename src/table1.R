library(ProjectTemplate)
load.project()

fctrs <- c(
  "P_ASA",
  "ECI_index_sum_all",
  "CCI_index_quan_original",
  "Rx_index_index",
  "P_SurgYear"
)

dft1 <-
  df %>%
  mutate(
    Female = P_Gender == "Kvinna",
    P_ASA = factor(P_ASA, 1:4, c(1:3, "4-5"))
  ) %>%
  select(
    death90f,
    Female,
    one_of(predictors),
    contains("_index_"),
    starts_with("c_"),
    starts_with("ECI_"),
    starts_with("Rx_"),
    -P_Gender
  )

t1 <-
  tableone::CreateTableOne(
    strata = "death90f",
    vars = setdiff(names(dft1), "death90f"),
    factorVars = fctrs,
    data = dft1,
    test = FALSE
  )

table1 <-
  t1 %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble(rownames = "what") %>%
  mutate(
    what = gsub("= TRUE|P_|c_|TypeOf|P_Surg|ECI_|Rx_", "", what),
    what = gsub("_", " ", what)
  ) %>%
  add_row(what = "Comorbidity indices",      .after = 31) %>%
  add_row(what = "Combined comorbidities",   .after = 52) %>%
  add_row(what = "Elixhauser comorbidities", .after = 69) %>%
  add_row(what = "Risk Rx V comorbidities", .after = 75)

cache_all(c("t1", "table1"))
