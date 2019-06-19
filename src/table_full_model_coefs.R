library(ProjectTemplate)
load.project()

table_full_model_coefs <-
  modsums$modsum[[1]]$coefmat.full %>%
  as_tibble(rownames = "variables") %>%
  mutate(
    coef = Estimate,
    lo   = Estimate - 1.96 * `Std. Error`,
    hi   = Estimate + 1.96 * `Std. Error`
  ) %>%
  mutate_at(vars(Estimate, lo, hi), exp) %>%
  transmute(
    variables = gsub("c_|P_|TRUE|Sex_", "", variables),
    variables = gsub("ASA_X1", "ASA 1", variables),
    variables = gsub("ASA_X2", "ASA 2", variables),
    variables = gsub("ASA_X3", "ASA 3", variables),
    variables = gsub("_", " ", variables),
    OR        = Estimate,
    CI        = sprintf("%.2f-%.2f", lo, hi),
    p         = ifelse(`Pr(>|z|)` < 0.001, "< 0.001", round(`Pr(>|z|)`, 2))
  ) %>%
  filter(variables != "(Intercept)") %>%
  arrange(variables)

  cache("table_full_model_coefs")
