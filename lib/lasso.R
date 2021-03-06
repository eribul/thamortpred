# Apply lasso regression with CV for best lambda and return coeficient estimates
lasso <- function(x) {
  fit <- glmnet::cv.glmnet(
    select(x, -death90f) %>% as.matrix(),
    x$death90f,
    family = "binomial"
  )
  as_tibble(
    as.matrix(coef(fit$glmnet.fit, s = fit$lambda.min)),
    rownames = "variable"
  ) %>%
    rename(coef = `1`) %>%
    filter(variable != "(Intercept)")
}
