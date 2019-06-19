library(ProjectTemplate)
load.project()

table_auc <-
  aucs %>%
  tidyr::extract(
    name, c("model", "age"),
    "([\\w- \\+]*) ?\\(?([\\w ]*)\\)?", perl = TRUE,
    remove = FALSE
  ) %>%
  mutate(
    dimension = ifelse(grepl("|", preds, fixed = TRUE), "Multivariable", "Univariable"),
    model = trimws(model),
    age   = replace(age, age == "" & model == "full", "main"),
    AUC   = sprintf("%.2f (%.2f-%.2f)", AUCtrain_est, AUCtrain_lo, AUCtrain_hi)
  ) %>%
  #group_by(model) %>%
  arrange(dimension, model) %>%
  mutate(
    dimension = ifelse(duplicated(dimension), "", dimension),
    model = replace(model, duplicated(model), ""),
  ) %>%
  select(dimension, model, age, AUC)

cache("table_auc")
