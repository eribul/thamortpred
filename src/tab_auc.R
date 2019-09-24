library(ProjectTemplate)
load.project()


brlasso_tbl_auc <-
  all_models %>%
  arrange(desc(AUC_est)) %>%
  transmute(
    Model,
    AUC   = sprintf("%.2f (%.2f-%.2f)", AUC_est, AUC_lo, AUC_hi)
  )

cache("brlasso_tbl_auc")
