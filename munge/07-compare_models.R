# "unregularized least-square fit restricted to variables in J" /[@Bach2008]
form <- function(nms) paste("death90f ~", paste(nms, collapse = " + "))

# Splines for age - Har testat med både 2 och 3 knots!
# Ingen skillnad så behåller med 3
fns3 <-
  paste(
    "death90f ~",
    paste(setdiff(best_coefs_75, "P_Age"), collapse = " + "),
    " + splines::ns(P_Age, 3)"
  )

glmdf_model <- function(...) glm(..., data = df_model, family = binomial())
glmdf  <- function(...) glm(..., data = df, family = binomial())

all_models_tmp <-
  tribble(
    ~Model,           ~fig, ~fit,
    "BRLasso all",     FALSE, glmdf_model(form(best_coefs_all)),
    "BRLasso any",     FALSE, glmdf_model(form(best_coefs_any)),
    "BRLasso 75%",     TRUE,  glmdf_model(form(best_coefs_75)),
    "BRLasso 75% ns3", FALSE, glmdf_model(fns3),
    "CCI",             TRUE,  glmdf(death90f ~ CCI_index_quan_original),
    "ECI",             TRUE,  glmdf(death90f ~ ECI_index_sum_all),
    "ASA",             TRUE,  glmdf(death90f ~ P_ASA),
    "Age and sex",     TRUE,  glmdf(death90f ~ P_Age + P_Gender),
  )

all_models <-
  all_models_tmp %>%
  mutate(
    tidy      = map(fit, broom::tidy),
    AIC       = map_dbl(fit, AIC),
    pred      = map(fit, predict, type = "response"),
    obspred   = map(pred, ~ tibble(pred = ., obs = y)),
    ROC       = map(obspred, pROC::roc, "obs", "pred", direction = "<"),
    AUCci     = map(ROC, pROC::ci.auc),
    AUC_lo    = map_dbl(AUCci, 1),
    AUC_est   = map_dbl(AUCci, 2),
    AUC_hi    = map_dbl(AUCci, 3),
    roc_auc   = map(obspred, roc_auc, obs, pred,   options = list(direction = ">")),
    roc_auc   = map_dbl(roc_auc, ".estimate"),
    roc_curve = map(obspred, roc_curve, obs, pred, options = list(direction = ">"))
  )

cache("all_models")
