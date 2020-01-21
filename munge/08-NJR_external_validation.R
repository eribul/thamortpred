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
