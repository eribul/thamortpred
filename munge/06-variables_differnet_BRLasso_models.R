
# Table to report with no of selections per variable
brlasso_tbl_selected <-
  best_coefs_tmp %>%
  unnest(coefs) %>%
  count(variable, sort = TRUE)

cache("brlasso_tbl_selected")

# List of variables selected in at least 75 % of the cases
best_coefs_75 <-
  brlasso_tbl_selected %>%
  filter(n >= round(config$Bmax * .75)) %>% # Urspr föreslogs intersect (variabler som tas varje gång)
  select(variable) %>%
  pluck(1)

cache("best_coefs_75")

# All variables ever selected
best_coefs_any <-
  brlasso_tbl_selected %>%
  select(variable) %>%
  pluck(1)

cache("best_coefs_any")

# All variables selected each time (as original article proposed)
best_coefs_all <-
  brlasso_tbl_selected %>%
  filter(n == config$Bmax) %>%
  select(variable) %>%
  pluck(1)

cache("best_coefs_all")

