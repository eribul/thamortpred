library(ProjectTemplate)
load.project()


# Settings ----------------------------------------------------------------

options('na.action' = "na.fail")
memory.limit(1e10)
set.seed(132456798)
future::plan("multiprocess")
N_bost    <- 1000 # No of Bootstrap replicates
threshold <- .75 # Proportion of models including variable for it to be choosen


# Data --------------------------------------------------------------------

# Use training and evaluation data
data_split <- initial_split(df, strata = "death90f", p = 0.9)
df_train   <- training(data_split)
df_test    <- testing(data_split)



# Help functions ----------------------------------------------------------

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

reci <- function(df, outcome = "death90f") {
  rec <- recipe(as.formula(paste(outcome, "~ .")), df) %>%
    step_downsample(all_outcomes()) %>%
    step_nzv(all_predictors(), options = list(freq_cut = 99 / 1), skip = TRUE)
  if (sum(vapply(df, is.factor, logical(1))) > 1)
    rec <- rec %>% step_dummy(all_predictors(), -all_numeric())
  rec
}


# Use bootstrap resampling to find number of times each variable is selected in
# stepwise regression
find_predictors <- function(
  df, rec = reci(df), B = N_bost, outcome = "death90f") {

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
list_predictors <- function(x, df, outcome = "death90f", thr = threshold) {

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
  select(df_train, death90f, starts_with(prefix), -matches("index"))
}



# Screen all comorbidity measures as well as other factors for
# predictors to include in combined model
important_factors <-
  tibble( preds = c("ECI", "CCI", "Rx")) %>%
  mutate(data = map(preds, getdata)) %>%
  add_row(
    preds = "general",
    data = list(select(df_train, death90f, predictors))) %>%
  mutate(
    facts   = map(data, find_predictors, B = N_bost),
    impfact = map2(facts, data, list_predictors, outcome = "death90f")
  )

# Character vector of names of candidate predictors
candidates <- c(map(important_factors$impfact, "winners"), recursive = TRUE)
drop       <- c(map(important_factors$impfact, "loosers"), recursive = TRUE)

cache("important_factors")
cache("candidates")
cache("drop")


# Proportions of all variables --------------------------------------------

prop_selected <-
  important_factors$facts %>%
  map(summarise_at, vars(-id), mean) %>%
  c(recursive = TRUE) %>%
  enframe() %>%
  mutate(value = value * 100) %>%
  arrange(desc(value))

cache("prop_selected")

# Combine correlatied variables to new predictors -------------------------

# Prepare recipe to use for both training and evaluation data
bestrec <- function(df) {
  reci(df) %>%
  step_mutate(
    heart_infarct  = CCI_myocardial_infarction | Rx_angina,
    arrythmia      = ECI_cardiac_arrhythmias | Rx_arrhythmia,
    tumour         = ECI_solid_tumor | CCI_malingnancy | Rx_malignancies,
    hypothyroidism = ECI_hypothyroidism | Rx_hyperthyroidism
  ) %>%

  # Drop original components of new combined predictors
  step_rm(
    one_of(drop),
    CCI_myocardial_infarction, Rx_angina,
    ECI_cardiac_arrhythmias, Rx_arrhythmia,
    ECI_solid_tumor, CCI_malingnancy, Rx_malignancies,
    ECI_hypothyroidism, Rx_hyperthyroidism
  )
}




# Best model --------------------------------------------------------------

# We have found a candidate list of predictors to include.
# It is important that those are not strongly correlatred!

# Model averaging with model weights based on AIC from bootstrap replicates
# Note that each BS replicate is taken after the recipi is applied.
# The downsmapling means that this might be of limited value, wherefore an
# additional BS is applied below. We use sqrt(B) outer bootstrappinug replicates
# and sqrt(B) inner replicates
bsavg <- function(mod_data) {
  model <- glm(death90f ~ ., binomial, mod_data, x = TRUE, na.action = "na.fail")
  dr    <- MuMIn::dredge(model)
  am    <- model.avg(dr, data = mod_data)
  Weights(am) <- bootWeights(am, R = floor(sqrt(N_bost)))
  am
}


# Fit model based on variable names and average results based on bootstrap
mod_avg <- function(nms) {

  mod_avg <-
    df_train %>%
    # sqrt(B) outer bootstrap replicates
    rsample::bootstraps(ceiling(sqrt(N_bost)), strata = "death90f", apparent = TRUE) %>%
    mutate(
      recipes  = map(splits, prepper, recipe = bestrec(df_train), retain = TRUE),
      # Only keep variables that are matched by the candidate vector names
      mod_data = map(recipes, juice, death90f, matches(nms)),
      am       = future_map(mod_data, bsavg),
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

# Cancidate names prepared for reular expression matching
nms <- paste(candidates, collapse = "|")
nms_simp <- paste(setdiff(
  candidates, c("Rx_index_index", "Rx_inflammation_pain")), collapse = "|")

# Full and simplified model

# Simple glm with result ni list, help function to below
glml <- function(nms) {
  lr(juice(prep(reci(df_train)), death90f, matches(nms)))
}


pred <- function(..., model) {
  pred_type <- switch(class(model)[1], "_glm" = "prob", averaging = "response")
  p <- predict(model, ..., type = pred_type)
  if (inherits(model, "_glm")) p <- 1 - p$.pred_alive
  p
}


# Evaluate the final model based on ROC-curves and AUC
evaluate <- function(model, nms, rec) {

  df_train %>%
  rsample::bootstraps(N_bost, strata = "death90f", apparent = TRUE) %>%
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


# Evaluate model on test data
evaluate_test <- function(model, rec) {
  d <- bake(prep(rec), df_test)
  obspred <- tibble(pred = pred(d, model = model), obs = d$death90f)
  list(
    obspred   = obspred,
    auc       = roc_auc(obspred, obs, pred)$.estimate,
    roc_curve = roc_curve(obspred, obs, pred)
  )
}

# Object with models to compare and evaluate
models <-
  tibble(
    name  = c("full", "simp", "ASA", "CCI", "ECI", "Rx", "agesex"),
    preds = c(
      nms, nms_simp, "P_ASA", "CCI_index_quan_original",
      "ECI_index_walraven", "Rx_index_index", "P_Age|P_Gender"),
    modavg = c(TRUE, TRUE, logical(5))
  ) %>%
  mutate(

    # Functions and recipes for model fitting (model averaging or GLM)
    fun          = map(modavg, ~ {if (.) mod_avg else glml}),
    rec          = map(modavg, ~ {if (.) bestrec(df_train) else reci(df_train)}),
    # Model
    model        = map2(fun, preds, ~ .x(.y)),
    modsum       = map(model, summary),
    # Evaluate
    evaluate     = pmap(tibble(model, preds, rec), ~ evaluate(..1, ..2, ..3)),
    evalfinal    = map2(model, rec, evaluate_test),
    # AUC values from evaluation sets
    AUCtrain_lo  = map_dbl(evaluate, ~ quantile(.$auc, .025)),
    AUCtrain_est = map_dbl(evaluate, ~ quantile(.$auc, .5)),
    AUCtrain_hi  = map_dbl(evaluate, ~ quantile(.$auc, .975)),
    AUCfinal     = map_dbl(evalfinal, "auc")
  )


# Cache model data --------------------------------------------------------

aucs <-
  models %>%
  select(name, preds, starts_with("AUC"))
cache("aucs")

rocs <-
  models %>%
  transmute(
    name         = name,
    tr_obspred   = map(evaluate,  ~ bind_rows(.$obspred)),
    tr_roc_curve = map(evaluate,  ~ bind_rows(.$roc_curve)),
    te_obspred   = map(evalfinal, ~ bind_rows(.$obspred)),
    te_roc_curve = map(evalfinal, ~ bind_rows(.$roc_curve))
  )

modsums <- models %>% select(name, modsum)

cache("rocs")
cache("modsums")


# Figures and tables ------------------------------------------------------

# ROC plot for all resamples

# We have more data than we need for a plot. Simplify by loess model
# if more than 50 uniqe points on either axis
loesspreds <- function(data) {
  if (length(unique(data$sensitivity)) < 50 |
      length(unique(data$specificity)) < 50) {
    return(data)
  }
  spec <- seq(0, 1, 0.01)
  sens <- loess(sensitivity ~ specificity, data) %>%
    predict(spec)
  tibble(specificity = spec, sensitivity = sens)
}

# Make data set for plotting
roc_curves <-
  rocs %>%
  select(name, ends_with("roc_curve")) %>%
  gather("stage", "roc_curve", -name) %>%
  mutate(
    stage = if_else(startsWith(stage, "tr"), "Training", "Evaluation") %>%
      factor(c("Training", "Evaluation")),
    roc_curve = map(roc_curve, loesspreds)
  ) %>%
  unnest(roc_curve)

cache("roc_curves")

# Plot it!
roc_curves %>%
  ggplot(aes(1 - specificity, sensitivity, col = name, group = name)) +
  geom_path(size = 2) +
  geom_abline(lty = 3) +
  facet_grid(~ stage) +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graphs/rocs.png")


# Make data set for plotting
obspreds <-
  rocs %>%
  select(name, ends_with("obspred")) %>%
  gather("stage", "obspred", -name) %>%
  mutate(
    stage = if_else(startsWith(stage, "tr"), "Training", "Evaluation") %>%
      factor(c("Training", "Evaluation"))#,
    #roc_curve = map(roc_curve, loesspreds)
  ) %>%
  unnest(obspred)

cache("obspreds")


# Difference in estimated probabilities for dead and alive
# Note that y-axis missleading due to downsampling!
obspreds %>%
  rename("Observed survival" = obs) %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(y = ..density.., fill = `Observed survival`),
    alpha = .3, position = "identity"
  ) +
  geom_density(aes(col = `Observed survival`)) +
  facet_wrap(~ name + stage, scales = "free", labeller = function(l) label_value(l, multi_line = FALSE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("Predicted probability of death") +
  ylab("") +
  scale_y_continuous(breaks = NULL)

ggsave("graphs/separation_auc.png")


# AUC

# Fig
aucs %>%
  mutate(Model = fct_reorder(name, AUCfinal)) %>%
  ggplot(aes(Model, AUCtrain_est)) +
  geom_hline(aes(yintercept = .5), linetype = 2, col = "grey", size = 2) +
  geom_pointrange(aes(ymin = AUCtrain_lo, ymax = AUCtrain_hi), size = 1.5) +
  geom_point(aes(y = AUCfinal), color = "red", shape = 23, size = 4, fill = "red") +
  coord_flip() +
  theme_minimal() +
  ylab("AUC") +
  scale_y_continuous(breaks = seq(0, 1, .1))

ggsave("graphs/auc_ci.png")

# Table
auc_table <-
  aucs %>%
  arrange(desc(AUCfinal)) %>%
  mutate_if(is.numeric, round, 2) %>%
  transmute(
    Model = name,
    AUC_train = paste0(AUCtrain_est, " (", AUCtrain_lo, ", ", AUCtrain_hi, ")"),
    AUC_test = AUCfinal
  )
cache("auc_table")
