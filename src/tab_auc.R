suppressMessages({library(ProjectTemplate); load.project()})

brlasso_tbl_auc <-
  all_models %>%
  filter(tab) %>%
  arrange(desc(AUC_est)) %>%
  transmute(
    Model,
    AUC   = sprintf("%.2f (%.2f-%.2f)", AUC_est, AUC_lo, AUC_hi),
    # good  = if_else(AUC_lo > .7, "*", "")
  )

cache("brlasso_tbl_auc")