
# Table to report with no of selections per variable
brlasso_tbl_selected <-
  best_coefs_tmp %>%
  unnest(coefs) %>%
  count(variable, sort = TRUE) %>%

  # Should not be needed if everything reruned:
  mutate(
    variable =
      case_when(
        variable == "ECI_obesity_TRUE." ~ "c_obesity_TRUE.",
        variable == "c_cns_TRUE." ~ "c_cns_disease_TRUE.",
        TRUE ~ variable
      )
  )

cache("brlasso_tbl_selected")

# List of variables selected in at least 75 % of the cases
best_coefs_reduced <-
  brlasso_tbl_selected %>%
  filter(n >= round(config$Bmax * .33)) %>% # Urspr föreslogs intersect (variabler som tas varje gång)
  select(variable) %>%
  pluck(1)

cache("best_coefs_reduced")

# All variables ever selected
best_coefs <-
  brlasso_tbl_selected %>%
  select(variable) %>%
  pluck(1)

cache("best_coefs")
