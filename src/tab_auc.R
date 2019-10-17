suppressMessages({library(ProjectTemplate); load.project()})

brlasso_tbl_auc <-
  all_models %>%
  filter(tab) %>%
  arrange(desc(AUC_est)) %>%
  transmute(
    Model,
    AUC      = sprintf("%.2f (%.2f-%.2f)", AUC_est, AUC_lo, AUC_hi),
    `AUC corrected for optimism` = sprintf("%.2f (%.2f-%.2f)", AUC_est.corr, AUC_lo.corr, AUC_hi.corr)
  )

cache("brlasso_tbl_auc")
