# "unregularized least-square fit restricted to variables in J" /[@Bach2008]
form <- function(nms) paste("death90f ~", paste(gsub("_TRUE.|_X[23]|_Man", "", nms), collapse = " + "))

# Splines for age - Har testat med både 2 och 3 knots!
# Ingen skillnad så behåller med 3
fns3 <- function(coefs) {
  paste0(form(setdiff(coefs, "P_Age")), " + splines::ns(P_Age, 3)")
}


glmdf <- function(f){
  list(
    glm = glm(f, data = df, family = binomial()),
    lrm = rms::lrm(as.formula(f), data = df, x = TRUE, y = TRUE)
  )
}

all_models_tmp <-
  tribble(
    ~Model,                                            ~tab, ~fig,   ~fit,
    "BRL reduced (age as main effect)",                TRUE,  TRUE,  glmdf(form(best_coefs_reduced)),
    "BRL reduced (age as RCS)",                        TRUE,  FALSE, glmdf(fns3(best_coefs_reduced)),
    "BRL reduced without cancer (age as main effect)", TRUE,  FALSE, glmdf(form(setdiff(best_coefs_reduced, "c_cancer_TRUE."))),
    "BRL (age as main effect)",                        TRUE,  TRUE,  glmdf(form(best_coefs)),
    "BRL (age as RCS)",                                TRUE,  FALSE, glmdf(fns3(best_coefs)),
    "Charlson",                                        TRUE,  TRUE,  glmdf(death90f ~ CCI_index_quan_original),
    "Elixhauser",                                      TRUE,  TRUE,  glmdf(death90f ~ ECI_index_sum_all),
    "ASA",                                             TRUE,  TRUE,  glmdf(death90f ~ P_ASA),
    "Age and sex (age as main effect)",                TRUE,  TRUE,  glmdf(death90f ~ P_Age + P_Gender),
    "Age and sex (age as RCS)",                        TRUE,  FALSE, glmdf(death90f ~ splines::ns(P_Age, 3) + P_Gender)
  ) %>%
  mutate(
    glm = map(fit, pluck, "glm"),
    lrm = map(fit, pluck, "lrm")
  ) %>%
  select(-fit)


all_models <-
  all_models_tmp %>%
  mutate(
    tidy          = map(glm, broom::tidy, conf.int = TRUE, exponentiate = TRUE),
    AIC           = map_dbl(glm, AIC),
    pred          = map(glm, predict, type = "response"),
    obspred       = map(pred, ~ tibble(pred = ., obs = df$death90f)),
    ROC           = map(obspred, pROC::roc, "obs", "pred", levels = c("alive", "dead"), direction = "<"),
    AUCci         = furrr::future_map(ROC, pROC::ci.auc, method = "bootstrap", boot.stratified = FALSE, .progress = TRUE),
    AUC_lo        = map_dbl(AUCci, 1),
    AUC_est       = map_dbl(AUCci, 2),
    AUC_hi        = map_dbl(AUCci, 3),
    roc_auc       = map(obspred, yardstick::roc_auc, obs, pred,   options = list(direction = ">")),
    roc_auc       = map_dbl(roc_auc, ".estimate"),
    roc_curve     = map(obspred, yardstick::roc_curve, obs, pred, options = list(direction = ">")),

    lrm_validate  = furrr::future_map(lrm, rms::validate, B = 100),
    lrm_calibrate = furrr::future_map(lrm, calibrate),
    optimism      = map_dbl(lrm_validate, ~ .["Dxy", "optimism"]),
    AUC_est.corr  = AUC_est - optimism,
    AUC_lo.corr   = AUC_lo  - optimism,
    AUC_hi.corr   = AUC_hi  - optimism,

    cal_belt = future_map(
      obspred, ~ givitiR::givitiCalibrationBelt(
        as.numeric(.$obs == "dead"), .$pred, devel = "internal")
                           ),
    cal_belt.p = map_dbl(cal_belt, "p.value")
  ) %>%
  select(-pred, -ROC, -AUCci)

cache("all_models")



# Save best model for possible later use
fit_brl_reduced <- glmdf(form(best_coefs_reduced))
cache("fit_brl_reduced")

fit_brl_reduced_lean <- strip_glm(fit_brl_reduced$glm)
cache("fit_brl_reduced_lean")
