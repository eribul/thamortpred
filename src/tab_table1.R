suppressMessages({library(ProjectTemplate); load.project()})


# Prepare data ------------------------------------------------------------

dft1 <-
  df %>%
  mutate(
    P_ASA      = factor(P_ASA, as.character(1:3), c("I", "II", "III")),
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
  rename(`ASA class` = ASA)



# Table by survival status ------------------------------------------------

t1 <-
  tableone::CreateTableOne(
    strata = "Death90f",
    vars = setdiff(names(dft1), "Death90f"),
    factorVars = c("Sex", "ASA class", "Elixhauser", "Charlson"),
    data = dft1,
    test = FALSE
  )

cache("t1")


# Add total column --------------------------------------------------------

t1_all <-
  tableone::CreateTableOne(
    vars = setdiff(names(dft1), "Death90f"),
    factorVars = c("Sex", "ASA class", "Elixhauser", "Charlson"),
    data = dft1,
    test = FALSE
  ) %>%
  print(
    printToggle = FALSE
  ) %>%
  as_tibble()



# Combine -----------------------------------------------------------------

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
    what  = trimws(ifelse(level == "", gsub(" = TRUE", "", what), "")),
    what  = gsub("Peptiulcer", "Peptic ulcer", what),
    what  = gsub("Rheumatidisease", "Rheumatic disease", what),
  ) %>%
  mutate_at(vars(`Died within 90 days`,
                 `Survived at least 90 days`), zero) %>%
  select(what, level, `Died within 90 days`, `Survived at least 90 days`, Total)

cache("table1")




# NJR data ----------------------------------------------------------------

# NJR data was delivered in two Excel sheets.
# One with demographics
# And one with co-morbidities
# Those tables are read and transformed before merged wit hSHAR data

# First part with pre-computed variables ----------------------------------

t1_njr_1 <-
  readxl::read_excel(
    "data/descrip9odays mortality.xlsx",
    range = "B2:F16",
    col_names = c("what", "level", "Died within 90 days", "Survived at least 90 days", "Total")
  ) %>%
  mutate(
    level = replace(level, zoo::na.locf(what) == "ASA", c("I", "II", "III"))
  ) %>%
  mutate_all(coalesce, "") %>%
  # Empty row before ASA
  add_row(
    what = "ASA class (%)",
    level = "",
    `Died within 90 days` = "",
    `Survived at least 90 days` = "",
    Total = "",
    .after = 3           ### WARNING! Bad practice!
  ) %>%
  # Empty row before Charlson
  add_row(
    what = "Charlson (%)",
    level = "",
    `Died within 90 days` = "",
    `Survived at least 90 days` = "",
    Total = "",
    .after = 7           ### WARNING! Bad practice!
  ) %>%
  # Empty row before Elixhauser
  add_row(
    what = "Elixhauser (%)",
    level = "",
    `Died within 90 days` = "",
    `Survived at least 90 days` = "",
    Total = "",
    .after = 13           ### WARNING! Bad practice!
  ) %>%
  mutate(
    what = gsub("(ASA|Charlson|Elixhauser)$", "", what),
    what = case_when(
      what == "age" ~ "Age (mean (SD))",
      what == "female" ~ "Sex = Female (%)",
      TRUE ~ what
    )
  ) %>%
  mutate_all(~ gsub("([0-9])(\\()", "\\1 \\2", ., perl = TRUE)) %>%
  slice(1:7, 14:18, 8:13) ### WARNING! Bad practice!

cache("t1_njr_1")



# Second part with co-morbidities -----------------------------------------

t1_njr_2 <-
  readxl::read_excel(
    "data/descrip9odays mortality (1).xlsx",
    sheet = "New",
    skip = 1,
    col_names = c("what", "Died within 90 days", "Survived at least 90 days", "Total")) %>%
  mutate(
    what = paste(what, "(%)")
  ) %>%
  mutate_all(~ gsub("([0-9])(\\()", "\\1 \\2", ., perl = TRUE))


# Combine all parts -------------------------------------------------------

shar <-
  table1 %>%
  select(what, level, SHAR = Total) %>%
  mutate(what = zoo::na.locf(na_if(what, "")))

njr1 <-
  t1_njr_1 %>%
  select(what, level, NJR = Total) %>%
  mutate(what = zoo::na.locf(na_if(what, "")))

njr2 <-
  t1_njr_2 %>%
  select(what, NJR = Total)

table1_combined_tot <-
  shar %>%
  left_join(njr1, c("what", "level")) %>%
  left_join(njr2, "what") %>%
  transmute(
    what = if_else(duplicated(what), "", what),
    level,
    SHAR,
    NJR = coalesce(NJR.x, NJR.y, "")
  ) %>%
  mutate_at(vars(SHAR, NJR), ~gsub("(\\d{1,3})(\\d{3})", "\\1,\\2", .))

cache("table1_combined_tot")
