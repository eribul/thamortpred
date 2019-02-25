library(ProjectTemplate)
load.project()


# Table 2 unadjusted ------------------------------------------------------

lrm <- function(rhs) {
  glm(as.formula(paste("death90 ~", rhs)), binomial, df) %>%
    glmCI(TRUE)
}

univariable <-
  tibble(
    name = predictors
  ) %>%
  mutate(
    coefs = map(name, lrm)
  ) %>%
  unnest(coefs) %>%
  mutate(
    var = map2_chr(name, var, ~gsub(.x, "", .y, fixed = TRUE)),
    name = ifelse(duplicated(name), "", name),
    CI  = paste0("(", round(low, 2), "-", round(high, 2), ")")
  ) %>%
  select(-low, -high)

cache("table2")
