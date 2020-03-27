library(ProjectTemplate)
load.project()

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
    what = "Elixhauser (%)",
    level = "",
    `Died within 90 days` = "",
    `Survived at least 90 days` = "",
    Total = "",
    .after = 7           ### WARNING! Bad practice!
  ) %>%
  # Empty row before Elixhauser
  add_row(
    what = "Charlson (%)",
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



