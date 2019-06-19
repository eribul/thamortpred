
# Find candidate predictors -----------------------------------------------

# Screen all comorbidity measures as well as other factors for
# predictors to include in combined model
important_factors <-
  tibble(preds = c("ECI", "CCI", "Rx", "c_")) %>%
  mutate(data = map(preds, getdata)) %>%
  add_row(
    preds = "general",
    data = list(select(df, death90f, predictors))
  ) %>%
  mutate(
    facts   = map(data, find_predictors, B = config$N_bots),
    impfact = map2(facts, data, list_predictors, outcome = "death90f", thr = .75)
  )

# Character vector of names of candidate predictors
candidates <- c(map(important_factors$impfact, "winners"), recursive = TRUE)
drop       <- c(map(important_factors$impfact, "loosers"), recursive = TRUE)

cache_all(c("important_factors", "candidates", "drop"))


# Proportions of all variables --------------------------------------------

prop_selected <-
  important_factors$facts %>%
  map(summarise_at, vars(-id), mean) %>%
  c(recursive = TRUE) %>%
  enframe() %>%
  mutate(value = value * 100) %>%
  arrange(desc(value))

cache("prop_selected")
