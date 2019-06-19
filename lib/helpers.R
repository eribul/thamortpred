con <- shar_linkage_con()

predictors <- c(
  "P_Age", "P_Sex", "P_BMI", "P_ASA", "P_TypeOfHospital", "P_SurgYear",
  "education", "civil_status"
)

# Cahe list of pobjects at once
cache_all <- Vectorize(cache, "variable")

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


# Change table text to labels that can be presented
prestext <- function(df) {
  gs <- function(x, pattern, replacement) gsub(pattern, replacement, x)
  df %>%
  rename(
    variable = 1,
    level = 2
  ) %>%
  mutate(
    level    = tolower(level),
    variable =
      variable %>%
      gs("Rx_index_index", "RxRiskV") %>%
      gs("P_|c_|Surg|TypeOf|_index_sum_all|_index_quan_original", "") %>%
      gs("_", " "),

    # Initial capital letter
    variable = paste0(toupper(substr(variable, 1, 1)), substring(variable, 2))
  )
}
