# Get names of important variables
selected_vars <- function(df, N_boot = config$N_bots) {

  # Perform lasso regression to bootstrap samples
  coefs_all <-
    df %>%
    rsample::bootstraps(N_boot) %>%
    mutate(
      data  = map(splits, as_tibble),
      lasso = map(data, lasso)
    ) %>%
    unnest(lasso) %>%
    group_by(variable) %>%
    summarise(
      impor = abs(mean(coef))
    ) %>%
    ungroup() %>%
    filter(impor > 0) %>%
    arrange(desc(impor)) %>%
    add_rowindex()

  # How many variables needed for good model
  break_p <-
    with(
      coefs_all,
      SiZer::piecewise.linear(.row, impor, 1)
    ) %>%
    {.$change.point} %>%
    round()

  # Keep only important variables
  coefs_all %>%
    slice(seq_len(break_p))
}
