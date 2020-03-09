suppressMessages({library(ProjectTemplate); load.project()})


njr_AUC <-
  tibble(
    Model   = "NJR (BRL reduced) (age as main effect)",
    AUC_lo  = njr_AUCci[1],
    AUC_est = njr_AUCci[2],
    AUC_hi  = njr_AUCci[3]
  )


brlasso_tbl_auc <-
  all_models %>%
  filter(tab) %>%
  bind_rows(njr_AUC) %>%
  arrange(desc(AUC_est)) %>%
  transmute(
    Model = model_names(Model, age = TRUE),
    AUC      = sprintf("%.2f (%.2f-%.2f)", AUC_est, AUC_lo, AUC_hi),
    #`AUC corrected for optimism` =
    #  sprintf("%.2f (%.2f-%.2f)", AUC_est.corr, AUC_lo.corr, AUC_hi.corr)
  ) %>%
  separate(Model, c("Model", "Age as"), " \\(age as ") %>%
  mutate(`Age as` = gsub("\\)", "", `Age as`))

cache("brlasso_tbl_auc")
