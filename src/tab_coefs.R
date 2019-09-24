library(ProjectTemplate)
load.project()


estimates <-
  all_models %>%
  filter(Model == "BRLasso 75%") %>%
  select(fit) %>%
  pluck(1, 1) %>%
  {left_join(glmCI(.), glmCI(., TRUE), "var", suffix = c(".beta", ".OR"))} %>%
  transmute(
    term = var,
    beta = sprintf("%.2f (%.2f-%.2f)", beta, low.beta, high.beta),
    OR   = sprintf("%.2f (%.2f-%.2f)", OR, low.OR, high.OR)
  )

p <-
  all_models %>%
  filter(Model == "BRLasso") %>%
  select(tidy) %>%
  pluck(1, 1) %>%
  transmute(
    term,
    p = pvalue_format()(p.value)
  )

brlasso_tbl_coefs <-
  left_join(estimates, p, "term") %>%
  mutate(
    term = gsub("[cP]_|_TRUE.|", "", term),
    term = gsub("_", " ", term),
  )

cache("brlasso_tbl_coefs")
