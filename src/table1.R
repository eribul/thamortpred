library(ProjectTemplate)
load.project()

fctrs <- c(
  "P_Sex",
  "P_ASA",
  "ECI",
  "CCI",
  "RxRiskV"#,
  #"P_SurgYear"
)

dft1 <-
  df %>%
  rename(
    CCI = CCI_index_quan_original,
    ECI = ECI_index_sum_all,
    RxRiskV = Rx_index_index
  ) %>%
  mutate(
    CCI     = replace(CCI, CCI == 4, "4+"),
    ECI     = replace(ECI, ECI == 3, "3+"),
    RxRiskV = replace(RxRiskV, RxRiskV == 7, "7+")
  ) %>%
  select(
    death90f,
    one_of(predictors),
    -P_SurgYear,
    CCI, ECI, RxRiskV,
    starts_with("c_"),
    starts_with("ECI_"),
    starts_with("Rx_")
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
    what = gsub("= TRUE|P_|c_|TypeOf|P_Surg|index", "", what),
    what = gsub("_", " ", what),
    level = ifelse(startsWith(what, " "), what, ""),
    what = ifelse(level == "", what, "")
  ) %>%
  select(what, level, alive, dead)

cache_all(c("t1", "table1"))
