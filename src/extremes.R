library(ProjectTemplate)
load.project()


# Vi kikar lite på de mest extrema patietnerna vad gäller ECI.

# Extremes in Elixhauser included in training data but not in test data
ECI_extremes <-
  bind_rows(
    train = df_train,
    eval = df_test,
    .id = "dataset"
  ) %>%
  count(dataset, ECI_index_walraven) %>%
  spread(dataset, n)
cache("ECI_extremes")


# Find the most HEALTHY ECI-person
healthy <-
  df_train %>%
  filter(ECI_index_walraven == min(ECI_extremes$ECI_index_walraven))
# comorbids
healthy %>%
  select(starts_with("ECI_"), -ECI_index_walraven) %>%
  {names(.)[c(., recursive = TRUE)]}
# Outcome = alive
select(healthy, death90f)


# Find the most SICK ECI-person
sick <-
  df_train %>%
  filter(ECI_index_walraven == max(ECI_extremes$ECI_index_walraven))
# comorbids
sick %>%
  select(starts_with("ECI_"), -ECI_index_walraven) %>%
  {names(.)[c(., recursive = TRUE)]}
# Outcome = alive
select(sick, death90f)
