library(ProjectTemplate)
load.project()

# Table 2 unadjusted ------------------------------------------------------

lrm <- function(rhs) {
  glm(
    as.formula(paste("death90 ~", rhs)),
    binomial, df
  ) %>%
  glmCI(TRUE)
}

# Obs här använder vi objekt som skapas i modelling.R! Inte helt OK egentligen!
full_model_coefs <- gsub("TRUE|_Female", "", names(bestmodelcoefs)[-1])

vars <-
  unique(c(
    predictors,
    names(df)[grepl("index", names(df))],
    gsub("_X[0-9]", "", full_model_coefs)
  ))

# Proportions selected
props <-
  prop_selected %>%
  mutate(
    clean_name = gsub("[^[:alnum:]]|X", "", name),
  ) %>%
  select(-name)

t2_univariable <-
  tibble(name = vars) %>%
  mutate(
    model = ifelse(name %in% gsub("_X[23]", "", full_model_coefs), "FM", ""),
    coefs = map(name, lrm)
  ) %>%
  unnest(coefs) %>%
  mutate(clean_name = gsub("[^[:alnum:]]|TRUE", "", var)) %>%
  left_join(props, "clean_name") %>%
  mutate(
    var  = map2_chr(name, var, ~gsub(.x, "", .y, fixed = TRUE)),
    var = gsub("TRUE", "", var),
    name = ifelse(duplicated(name), "", name),
    name = gsub("P_|c_|index_sum_all|index_quan_original|index_index|TypeOf|Surg", "", name),
    name = gsub("Rx$", "RxRiskV", name),
    name = gsub("_", " ", name),
    CI   = paste0("(", round(low, 2), "-", round(high, 2), ")")
  ) %>%
  select(name, var, OR, CI, `selected (%)` = value, model)


cache("t2_univariable")
