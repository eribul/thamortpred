# Perform calculations (time consuming)
profvis::profvis({
  best_coefs_tmp <-
    tibble(B = seq_len(config$Bmax)) %>%
    mutate(
      coefs_all = future_map(B, ~ BR_lasso_coefs(df_model), .progress = TRUE),
      breaks    = map_dbl(coefs_all, break_p),
      coefs     = map2(coefs_all, breaks, ~slice(.x, seq_len(.y)))
    )
})

cache("best_coefs_tmp")
