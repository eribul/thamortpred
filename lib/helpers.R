predictors <- c(
  "P_Age", "P_Gender", "P_BMI", "P_ASA", "P_TypeOfHospital", "P_SurgYear",
  "education", "civil_status", "ECI_index_walraven",
  "CCI_index_quan_original", "Rx_index_index"
)

### Summary function for GLM
glmCI <- function(model, exponent = FALSE, alpha = 0.05) {
  co <- coef(model)
  l  <- qnorm(1 - alpha / 2) * sqrt(diag(summary(model)$cov.scaled))

  # calculuate CI for linear case
  mat <- tibble::tibble(var = names(co), beta = co, low = co - l, high = co + l)

  # if exponent=TRUE, exponeniate the coefficients and CIs
  if (exponent) {
    mat <- mutate_if(mat, is.numeric, exp)
    names(mat)[names(mat) == "beta"] <-
      switch(summary(model)$family$link, logit = "OR", log  = "RR")
  }
  filter(mat, var != "(Intercept)")
}


# Summarise survfit results
survEst <- function(fit, times) {
  x <- as.data.frame(summary(fit, times = times))
  ind <- c("time", "surv",  "lower", "upper")
  if (!is.null(fit$strata))
    ind <- c(ind, "strata")
  x[ind]
}

