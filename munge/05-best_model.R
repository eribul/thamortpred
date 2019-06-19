
# Cancidate names prepared for reular expression matching
nms <- paste(setdiff(candidates, "P_Age"), collapse = "|")

# Object with models to compare and evaluate
models <-
  tribble(
    ~name,                ~preds,                                                ~ modavg,
    "full",                paste0(nms, "|P_Age"),                                TRUE,
    "full (rcs 2)",        paste0(nms, "|P_Age_ns_[12]"),                        TRUE,
    "full (rcs 3)",        paste0(nms, "|P_Age_ns_[123]"),                       TRUE,
    "full-cancer",       paste(setdiff(candidates, "c_cancer"), collapse = "|"), TRUE,
    "simpl",               paste(setdiff(candidates, "P_ASA"), collapse = "|"),  TRUE,
    "ASA",                 "P_ASA",                                              FALSE,
    "CCI",                 "CCI_index_quan_original",                            FALSE,
    "ECI",                 "ECI_index_sum_all",                                  FALSE,
    "RxRiskV",             "Rx_index_index",                                     FALSE,
    "age + sex (main)",    "P_Sex|P_Age",                                        FALSE,
    "age + sex (rcs 2)",   "P_Sex|P_Age_ns_[12]",                                FALSE,
    "age + sex (rcs 3)",   "P_Sex|P_Age_ns_[123]",                               FALSE
  ) %>%
  mutate(

    # Functions and recipes for model fitting (model averaging or GLM)
    fun          = map(modavg, ~ {if (.) mod_avg else glml}),
    rec          = map(name, ~ reci(df, rcs = case_when(
      grepl("2", .) ~ 2,
      grepl("3", .) ~ 3,
      TRUE ~ NA_real_)
    )
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
# To much data to save everything. Keep only relevant parts!

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

cache_all(c("rocs", "modsums", "bestmodelcoefs"))
