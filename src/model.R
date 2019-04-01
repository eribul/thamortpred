library(ProjectTemplate)
load.project()


# Settings ----------------------------------------------------------------

options('na.action' = "na.fail")
memory.limit(1e10)
set.seed(132456798)
future::plan("multiprocess")
N_bots    <- 1000 # No of Bootstrap replicates
threshold <- .75 # Proportion of models including variable for it to be choosen



# Help functions ----------------------------------------------------------



# Screen all comorbidity measures as well as other factors for
# predictors to include in combined model
important_factors <-
  tibble(preds = c("ECI", "CCI", "Rx", "c_")) %>%
  mutate(data = map(preds, getdata)) %>%
  add_row(
    preds = "general",
    data = list(select(df, death90f, predictors))
  ) %>%
  mutate(
    facts   = map(data, find_predictors, B = N_bots),
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





# Best model --------------------------------------------------------------

# Cancidate names prepared for reular expression matching
nms <- paste(candidates, collapse = "|")

# Object with models to compare and evaluate
models <-
  tibble(
    name  = c(
      "full", "full_age2", "full_age3",
      "ASA", "CCI", "ECI", "Rx",
      "sex_age", "sex_age2", "sex_age3"),
    preds = c(
      rep(nms, 3),
      "P_ASA", "CCI_index_quan_original", "ECI_index_sum_all", "Rx_index_index",
      rep("P_Age|P_Gender", 3)),
    modavg = c(rep(TRUE, 3), logical(7))
  ) %>%
  mutate(

    # Functions and recipes for model fitting (model averaging or GLM)
    fun          = map(modavg, ~ {if (.) mod_avg else glml}),
    rec          = map(name,   ~ {
      if (!grepl("[23]$", name)) reci(df)
      else reci(df) + step_ns(P_Age, deg_free = 2)}
      ),
    # Model
    model        = future_map2(fun, preds, ~ .x(.y)),
    modsum       = map2(modavg, model,
                        ~ {if (.x) summary(.y) else summary(.y$fit)}),
    # Evaluate
    evaluate     = pmap(tibble(model, preds, rec), ~ evaluate(..1, ..2, ..3)),
    # AUC values from evaluation sets
    AUCtrain_lo  = map_dbl(evaluate, ~ quantile(.$auc, .025)),
    AUCtrain_est = map_dbl(evaluate, ~ quantile(.$auc, .5)),
    AUCtrain_hi  = map_dbl(evaluate, ~ quantile(.$auc, .975)),
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
    tr_roc_curve = map(evaluate,  ~ bind_rows(.$roc_curve))
  )

modsums <- models %>% select(name, modsum)

# Estimated coefficients from full model
bestmodelcoefs <- filter(models, name == "full")$model[[1]]$coefficients[1, ]

cache("rocs")
cache("modsums")
cache("bestmodelcoefs")






# Figures and tables ------------------------------------------------------

# Make data set for plotting
roc_curves <-
  rocs %>%
  select(name, ends_with("roc_curve")) %>%
  gather("stage", "roc_curve", -name) %>%
  mutate(
    roc_curve = map(roc_curve, loesspreds)
  ) %>%
  unnest(roc_curve)

cache("roc_curves")

# Plot it!
  ggplot(
    filter(roc_curves, name != "full"),
    aes(1 - specificity, sensitivity, col = name)
  ) +
  facet_wrap(~ name) +
  geom_path(size = 2) +
  geom_path(
    aes(1 - specificity, sensitivity),
    filter(roc_curves, name == "full") %>% select(specificity, sensitivity),
    size = 2, col = "black"
  ) +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graphs/rocs.png")


# Make data set for plotting
obspreds <-
  rocs %>%
  unnest(tr_obspred)

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
  facet_wrap(~ name, scales = "free", labeller = function(l) label_value(l, multi_line = FALSE)) +
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
  mutate(Model = fct_reorder(name, AUCtrain_est)) %>%
  ggplot(aes(Model, AUCtrain_est)) +
  geom_hline(aes(yintercept = .5), linetype = 2, col = "grey", size = 2) +
  geom_pointrange(aes(ymin = AUCtrain_lo, ymax = AUCtrain_hi), size = 1.5) +
  coord_flip() +
  theme_minimal() +
  ylab("AUC") +
  scale_y_continuous(breaks = seq(0, 1, .1))

ggsave("graphs/auc_ci.png")

# Table
auc_table <-
  aucs %>%
  arrange(desc(AUCtrain_est)) %>%
  mutate_if(is.numeric, round, 2) %>%
  transmute(
    Model = name,
    AUC_train = paste0(AUCtrain_est, " (", AUCtrain_lo, ", ", AUCtrain_hi, ")")
  )
cache("auc_table")
