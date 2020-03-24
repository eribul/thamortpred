doParallel::registerDoParallel()
# load("data/export_obspred_from_njr.RData") # Data from Erik L

roccurve <- function(x) pROC::roc(x, "obs", "pred", direction = "<")
aucci    <- function(x) pROC::ci.auc(x, method = "bootstrap", boot.stratified = FALSE, parallel = TRUE)
cal      <- function(x) givitiR::givitiCalibrationBelt(x$obs, x$pred, devel = "external")


# Model as is
njr_ROC         <- roccurve(obspred); cache("njr_ROC")
njr_AUCci       <- aucci(njr_ROC);    cache("njr_AUCci")
njr_calibration <- cal(obspred);      cache("njr_calibration")

# Model re-calibrated
njr_ROC3         <- roccurve(obspred3); cache("njr_ROC3")
njr_AUCci3       <- aucci(njr_ROC3);    cache("njr_AUCci3")
njr_calibration3 <- cal(obspred3);      cache("njr_calibration3")

# Numbers to present
N_njr <- list(
  death = length(njr_ROC$cases),
  surv  = length(njr_ROC$controls)
  )

N_njr <- within(N_njr, {tot = death + surv})

cache("N_njr")


# 90 day survival ---------------------------------------------------------

# We assume there is no censoring and therefore base the estimate on bootstrap
njr_surv <-
  tibble(
    status = c(numeric(N_njr$death) + 1, numeric(N_njr$surv))
  ) %>%
  rsample::bootstraps(100) %>%
  mutate(
    death = map_dbl(splits, ~ mean(as_tibble(.)$status))
  ) %>%
  summarise(
    est = mean(death),
    lo = quantile(death, .025),
    hi = quantile(death, .975),
    mort = sprintf("%0.2f %% (95 %% CI: %0.2f to %0.2f)", est * 100, lo * 100, hi * 100)
  ) %>%
  select(mort) %>%
  pluck(1, 1)

cache("njr_surv")
