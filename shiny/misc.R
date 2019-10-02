# file.copy("cache/fit_brl_any_lean.RData", "shiny/fit_brl_any_lean.RData", TRUE)
load("fit_brl_any_lean.RData")

coefs <-
  tibble(coef = names(coef(fit_brl_any_lean))[-1]) %>%
  mutate(
    checkbox     = grepl("TRUE", coef),
    coef_name    = gsub("(TRUE|Man|[23])$", "", coef),
    coef_present = gsub("[cP]_|ECI_", "", coef_name),
    coef_present = gsub("_", " ", coef_present),
    coef_present = gsub("Gender", "Male sex", coef_present)
  ) %>%
  select(-coef) %>%
  distinct(.keep_all = TRUE)
