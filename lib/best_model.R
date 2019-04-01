
# Find best mdoel ---------------------------------------------------------

# Model averaging with model weights based on AIC from bootstrap replicates
# Note that each BS replicate is taken after the recipi is applied.
# The downsmapling means that this might be of limited value, wherefore an
# additional BS is applied below. We use sqrt(B) outer bootstrappinug replicates
# and sqrt(B) inner replicates
bsavg <- function(mod_data) {
  model <- glm(death90f ~ .,
               binomial, mod_data, x = TRUE, na.action = "na.fail")
  dr          <- MuMIn::dredge(model)
  am          <- model.avg(dr, data = mod_data)
  Weights(am) <- bootWeights(am, R = floor(sqrt(config$N_bots)))
  am
}


# Fit model based on variable names and average results based on bootstrap
mod_avg <- function(nms) {

  mod_avg <-
    df %>%
    # sqrt(B) outer bootstrap replicates
    rsample::bootstraps(
      ceiling(sqrt(config$N_bots)), strata = "death90f", apparent = TRUE) %>%
    mutate(
      recipes  = map(splits, prepper, recipe = reci(df), retain = TRUE),
      # Only keep variables that are matched by the candidate vector names
      mod_data = map(recipes, juice, death90f, matches(nms)),
      am       = map(mod_data, bsavg),
      w        = map(am, ~ enframe(c(Weights(.))))
    )

  # Average weights over the outer bootstrap replicates
  w <-
    mod_avg %>%
    unnest(w) %>%
    group_by(name) %>%
    summarise(w = mean(value))

  # Update weights for the whole training set wit hthe boostatrap averaged weights
  fit <- mod_avg$am[mod_avg$id == "Apparent"][[1]]
  Weights(fit) <- w$w
  fit
}


# Simple glm with result ni list, help function to below
glml <- function(nms) {
  lr(juice(prep(reci(df)), death90f, matches(nms)))
}


pred <- function(..., model) {
  pred_type <- switch(class(model)[1], "_glm" = "prob", averaging = "response")
  p <- predict(model, ..., type = pred_type)
  if (inherits(model, "_glm")) p <- 1 - p$.pred_alive
  p
}


# Evaluate the final model based on ROC-curves and AUC
evaluate <- function(model, nms, rec) {

  df %>%
    rsample::bootstraps(config$N_bots, strata = "death90f", apparent = TRUE) %>%
    mutate(
      recipes   = map(splits, prepper, recipe = rec, retain = TRUE),
      mod_data  = map(recipes, juice, death90f, matches(nms)),
      # Tibble with observed data from bootstrap samples and predicted using the
      # selected model from above applied to each bootstrap dataset
      p         = map(mod_data, pred, model = model),
      obspred   = map2(mod_data, p, ~ tibble(obs = .x$death90f, pred = .y)),

      auc       = map(obspred, roc_auc, obs, pred),
      auc       = map_dbl(auc, ".estimate"),
      roc_curve = map(obspred, roc_curve, obs, pred)
    )
}
