suppressMessages({library(ProjectTemplate); load.project()})

dft1 <-
  df %>%
  mutate(
    Charlson   = replace(CCI_index_quan_original, CCI_index_quan_original == 4, "4+"),
    Elixhauser = replace(ECI_index_sum_all, ECI_index_sum_all == 3, "3+"),
    death90f   = factor(death90f, c("dead", "alive"),
                        c("Died within 90 days", "Survived at least 90 days"))
  ) %>%
  select(
    death90f,
    P_Age,
    P_Sex,
    P_BMI,
    P_ASA,
    P_TypeOfHospital,
    education,
    civil_status,
    Charlson,
    Elixhauser,
    starts_with("c_"),
  ) %>%
  setNames(clean_names(names(.))) %>%
  rename(`ASA grade` = ASA)

t1 <-
  tableone::CreateTableOne(
    strata = "Death90f",
    vars = setdiff(names(dft1), "Death90f"),
    factorVars = c("Sex", "ASA grade", "Elixhauser", "Charlson"),
    data = dft1,
    test = FALSE
  )

cache("t1")


# Add total column --------------------------------------------------------

t1_all <-
  tableone::CreateTableOne(
    vars = setdiff(names(dft1), "Death90f"),
    factorVars = c("Sex", "ASA grade", "Elixhauser", "Charlson"),
    data = dft1,
    test = FALSE
  ) %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble()



zero <- function(x) {
  gsub("0 ( 0.0)", "0", x, fixed = TRUE) %>%
  {gsub("0.0", "<0.1", .)}
}

table1 <-
  t1 %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble(rownames = "what") %>%
  add_column(Total = t1_all$Overall) %>%
  mutate(
    level = trimws(ifelse(startsWith(what, " "), what, "")),
    level = paste0(toupper(substr(level, 1, 1)), substring(level, 2)),
    what = trimws(ifelse(level == "", gsub(" = TRUE", "", what), ""))
  ) %>%
  mutate_at(vars(`Died within 90 days`,
                 `Survived at least 90 days`), zero) %>%
  select(what, level, `Died within 90 days`, `Survived at least 90 days`, Total)

cache("table1")
