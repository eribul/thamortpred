# Perform calculations (time consuming)
best_coefs_tmp <-
  tibble(B = seq_len(config$Bmax)) %>%
  mutate(
    coefs = future_map(B, ~ selected_vars(df_model, config$N_bots), .progress = TRUE)
  )

cache("best_coefs_tmp")
