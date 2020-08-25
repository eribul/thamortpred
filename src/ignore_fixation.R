library(ProjectTemplate)
load.project()

fixation <-
  df_ignore_fixation %>%
  left_join(
    select(df_shpr_orig, LopNr, P_Side, P_FemStemCemMix, P_AcetCupCemMix),
    c("LopNr", "P_Side")
  ) %>%
  rename(c_cns_disease = c_cns_diseases) %>%
  mutate(
    fixation =
      case_when(
        P_FemStemCemMix == "Cementfritt" &
        P_AcetCupCemMix == "Cementfritt"    ~ "Non-cemented",
        P_AcetCupCemMix == "Cementfritt"    ~ "Hybrid",
        P_FemStemCemMix == "Cementfritt"    ~ "Reverse hybrid",
        TRUE                                ~ "Cemented"
      )
  ) %>%
  filter(fixation != "Cemented") %>%
  nest(data = everything()) %>%
  mutate(
    glm     = map(data, ~glm(form(best_coefs_reduced), data = ., family = binomial())),
    pred    = map(data, ~predict(fit_brl_reduced$glm, ., type = "response")),
    obspred = map2(data, pred, ~tibble(obs = .x$death90f, pred = .y)),
    roc     = map(obspred, pROC::roc, "obs", "pred",
                  levels = c("alive", "dead"), direction = "<"),
    AUCci   = map(roc, pROC::ci.auc, method = "bootstrap",
                  boot.stratified = FALSE)
  )


fixation$AUCci[[1]]
# 95% CI: 0.54-0.76 (2000 non-stratified bootstrap replicates)

# Calibration -------------------------------------------------------------

# At large
(fixation$obspred[[1]]$obs == "dead") %>% mean() # 0.0011
fixation$obspred[[1]]$pred %>% mean()            # 0.0014

cal_belt <-
  givitiR::givitiCalibrationBelt(
    as.numeric(fixation$obspred[[1]]$obs == "dead"),
    fixation$obspred[[1]]$pred,
    devel = "external"
  )

plot(cal_belt, xlim = c(0, .05), ylim = c(0, .05))
# Ser inte bra ut. Stor överskattning
