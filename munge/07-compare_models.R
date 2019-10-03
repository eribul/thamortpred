# "unregularized least-square fit restricted to variables in J" /[@Bach2008]
form <- function(nms) paste("death90f ~", paste(gsub("_TRUE.|_X3|_Man", "", nms), collapse = " + "))

# Splines for age - Har testat med både 2 och 3 knots!
# Ingen skillnad så behåller med 3
fns3 <- function(coefs) {
  paste0(form(setdiff(coefs, "P_Age")), " + splines::ns(P_Age, 3)")
}


glmdf       <- function(...) glm(..., data = df, family = binomial())

all_models_tmp <-
  tribble(
    ~Model,              ~tab, ~fig, ~fit,
    "BRL all",           TRUE,  FALSE, glmdf(form(best_coefs_all)),
    "BRL any",           TRUE,  TRUE,  glmdf(form(best_coefs_any)),
    "BRL any-cancer",    TRUE,  FALSE,  glmdf(form(setdiff(best_coefs_any, "c_cancer_TRUE."))),
    "BRL any (RCS)",     TRUE,  FALSE, glmdf(fns3(best_coefs_any)),
    "BRL 75%",           FALSE, FALSE, glmdf(form(best_coefs_75)),
    "BRL 75% (RCS)",     FALSE, FALSE, glmdf(fns3(best_coefs_75)),
    "CCI",               TRUE,  TRUE,  glmdf(death90f ~ CCI_index_quan_original),
    "ECI",               TRUE,  TRUE,  glmdf(death90f ~ ECI_index_sum_all),
    "ASA",               TRUE,  TRUE,  glmdf(death90f ~ P_ASA),
    "Age and sex",       TRUE,  TRUE,  glmdf(death90f ~ P_Age + P_Gender),
    "Age and sex (RCS)", TRUE,  FALSE, glmdf(death90f ~ splines::ns(P_Age, 3) + P_Gender)
  )


all_models <-
  all_models_tmp %>%
  mutate(
    tidy      = map(fit, broom::tidy, conf.int = TRUE, exponentiate = TRUE),
    AIC       = map_dbl(fit, AIC),
    pred      = map(fit, predict, type = "response"),
    obspred   = map(pred, ~ tibble(pred = ., obs = df$death90f)),
    ROC       = map(obspred, pROC::roc, "obs", "pred", levels = c("alive", "dead"), direction = "<"),
    AUCci     = map(ROC, pROC::ci.auc),
    AUC_lo    = map_dbl(AUCci, 1),
    AUC_est   = map_dbl(AUCci, 2),
    AUC_hi    = map_dbl(AUCci, 3),
    roc_auc   = map(obspred, roc_auc, obs, pred,   options = list(direction = ">")),
    roc_auc   = map_dbl(roc_auc, ".estimate"),
    roc_curve = map(obspred, roc_curve, obs, pred, options = list(direction = ">"))
  ) %>%
  select(-fit, -pred, -ROC, -AUCci)

cache("all_models")

# Save best model for possible later use
fit_brl_any <- glmdf(form(best_coefs_any))
cache("fit_brl_any")

fit_brl_any_lean <- strip_glm(fit_brl_any)
cache("fit_brl_any_lean")
