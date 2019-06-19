
# Find candidate predictors ----------------------------------------------


# Logistic regressoin using parsnip
lr <- function(x) {
  fit <-
    logistic_reg() %>%
    set_engine("glm") %>%
    fit(death90f ~ ., data = x)
}

# Identify variables used in the best model according to stepwise regression
bestfit <- function(x) {
  vars <- setdiff(names(x), "death90f")
  best <- stats::step(glm(death90f ~ ., binomial, x), trace = 0)

  tibble(
    var  = vars,
    # Fins name matches fom original names (no dummies)
    incl = map_lgl(vars, ~ any(startsWith(names(coef(best)), .)))
  )
}

reci <- function(df, outcome = "death90f", rcs = NULL) {
  rec <-
    recipe(as.formula(paste(outcome, "~ .")), df) %>%
    step_downsample(all_outcomes())

  # Create dummy variables for categorical data
  if (sum(vapply(df, is.factor, logical(1))) > 1)
    rec <- rec %>% step_dummy(all_predictors(), all_numeric())

  # RCS for age
  if (!is.null(rcs) && !is.na(rcs))
    rec <- rec %>% step_ns(matches("P_Age"), deg_free = rcs)
  rec
}


# Use bootstrap resampling to find number of times each variable is selected in
# stepwise regression
find_predictors <- function(
  df, rec = reci(df), B = config$N_bots, outcome = "death90f") {

  df %>%
    rsample::bootstraps(B, strata = outcome) %>%
    mutate(
      recipes  = map(splits, prepper, recipe = rec, retain = TRUE),
      mod_data = map(recipes, juice),
      included = future_map(mod_data, bestfit)
    ) %>%
    unnest(included) %>%
    spread(var, incl)
}

# List names of predictors selected at least threshold % of the time
list_predictors <- function(x, df, outcome = "death90f", thr = .9) {

  winners_all <-
    x %>%
    summarise_at(vars(-id), mean) %>%
    mutate_all(~ . >= thr) %>%
    c(recursive = TRUE) %>%
    {names(.)[.]}

  preds    <- setdiff(names(df), outcome)
  winners  <- preds[map_lgl(preds, ~ any(startsWith(winners_all, .)))]
  loosers  <- setdiff(preds, winners)
  extra    <- winners_all[!map_lgl(winners_all, ~ any(startsWith(., winners)))]

  list(winners = winners, loosers = loosers, extra = extra)
}

# Create training data with selected columns
getdata <- function(prefix) {
  select(df, death90f, starts_with(prefix), -matches("index"))
}
