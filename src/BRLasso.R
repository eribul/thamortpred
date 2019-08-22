library(ProjectTemplate)
load.project()
set.seed(123)

df0 <-
  df %>%
  select(
    -op_first, -op_last, -stime, -status,
    -contains("index"),
    -starts_with("death"), death90f
  )

y <- df0$death90f

# Some outcomes does not exist --------------------------------------------

# Can't really estimate effects for variables with 0 outcomes in any combination
comb_lgl <-
  df0 %>%
  select_if(is.logical) %>%
  gather() %>%
  nest(value) %>%
  mutate(
    data = map(data, add_column, y),
    data = map(data, count, y, value),
    noncomplete = map_lgl(data, ~nrow(.) < 4)
  ) %>%
  filter(noncomplete) %>%
  select(-noncomplete) %>%
  unnest(data) %>%
  mutate(
    value = if_else(value, "condition", "condition free")
  ) %>%
  unite("value_y", value, y) %>%
  spread(value_y, n)

comb_fct_tmp <-
  df0 %>%
  select_if(is.factor) %>%
  select(-starts_with("death"))

n_lvls <-
  comb_fct_tmp %>%
  summarise_all(n_distinct) %>%
  gather("key", "n_lvls")

# Finns inga faktorvariabler med saknade kombinationer!
# Inga levels med extremt få heller
# Detta framgår förstås även av table1
comb_fct <-
  comb_fct_tmp %>%
  gather() %>%
  nest(value) %>%
  left_join(n_lvls, "key") %>%
  mutate(
    data        = map(data, add_column, y),
    data        = map(data, count, y, value) #,
    # noncomplete = map2_lgl(data, n_lvls, ~ nrow(.x) < .y)
  ) %>%
  unnest(data) %>%
  spread(y, n) %>%
  mutate(
    key = if_else(duplicated(key), "", key)
  ) %>%
  select(-n_lvls)


# df without those variables ----------------------------------------------

df0 <-
  df0 %>%
  select(-one_of(comb_lgl$key), death90f) %>%
  mutate_if(is.logical, as.factor) # needed for recipe

df0 <-
  recipe(death90f ~., df0) %>%
  # step_ns(matches("P_Age"), deg_free = 3) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_predictors()) %>%
  prep(df0) %>%
  bake(df0)


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


# Get names of important variables
selected_vars <- function(df, N_boot = 100) {

  # Perform lasso regression to bootstrap samples
  coefs_all <-
    df %>%
    rsample::bootstraps(N_boot) %>%
    mutate(
      data  = map(splits, as_tibble),
      lasso = map(data, lasso)
    ) %>%
    unnest(lasso) %>%
    group_by(variable) %>%
    summarise(
      impor = abs(mean(coef))
    ) %>%
    ungroup() %>%
    filter(impor > 0) %>%
    arrange(desc(impor)) %>%
    add_rowindex()

  # How many variables needed for good model
  break_p <-
    with(
      coefs_all,
      SiZer::piecewise.linear(.row, impor, 1)
    ) %>%
    {.$change.point} %>%
    round()

  # Keep only important variables
  coefs_all %>%
    slice(seq_len(break_p))
}


# Coefficient estimates -------------------------------------------------------

# No of external reruns
Bmax <- 5

# Perform calculations (time consuming)
best_coefs_tmp <-
  tibble(B = seq_len(Bmax)) %>%
  mutate(
    coefs = future_map(B, ~ selected_vars(df0, config$N_bots), .progress = TRUE)
  )

cache("best_coefs_tmp")

# Table to report with no of selections per variable
brlasso_tbl_selected <-
  best_coefs_tmp %>%
  unnest(coefs) %>%
  count(variable, sort = TRUE)

cache("brlasso_tbl_selected")

# List of variables selected in at least 75 % of the cases
best_coefs_75 <-
  brlasso_tbl_selected %>%
  filter(n >= round(Bmax * .75)) %>% # Urspr föreslogs intersect (variabler som tas varje gång)
  select(variable) %>%
  pluck(1)

cache("best_coefs_75")

# All variables ever selected
best_coefs_any <-
  brlasso_tbl_selected %>%
  select(variable) %>%
  pluck(1)

cache("best_coefs_any")

# All variables selected each time (as original article proposed)
best_coefs_all <-
  brlasso_tbl_selected %>%
  filter(n == Bmax) %>%
  select(variable) %>%
  pluck(1)

cache("best_coefs_all")

# Skatta coefs ------------------------------------------------------------

# "unregularized least-square fit restricted to variables in J" /[@Bach2008]
form <- function(nms) paste("death90f ~", paste(nms, collapse = " + "))

# Splines for age - Har testat med både 2 och 3 knots!
# Ingen skillnad så behåller med 3
fns3 <-
  paste(
    "death90f ~",
    paste(setdiff(best_coefs_75, "P_Age"), collapse = " + "),
    " + splines::ns(P_Age, 3)"
  )

glmdf0 <- function(...) glm(..., data = df0, family = binomial())
glmdf  <- function(...) glm(..., data = df, family = binomial())

all_models_tmp <-
  tribble(
    ~Model,           ~fig, ~fit,
    "BRLasso all",     FALSE, glmdf0(form(best_coefs_all)),
    "BRLasso any",     FALSE, glmdf0(form(best_coefs_any)),
    "BRLasso 75%",     TRUE,  glmdf0(form(best_coefs_75)),
    "BRLasso 75% ns3", FALSE, glmdf0(fns3),
    "CCI",             TRUE,  glmdf(death90f ~ CCI_index_quan_original),
    "ECI",             TRUE,  glmdf(death90f ~ ECI_index_sum_all),
    "ASA",             TRUE,  glmdf(death90f ~ P_ASA),
    "Age and sex",     TRUE,  glmdf(death90f ~ P_Age + P_Gender),
  )

all_models <-
  all_models_tmp %>%
  mutate(
    tidy      = map(fit, broom::tidy),
    AIC       = map_dbl(fit, AIC),
    pred      = map(fit, predict, type = "response"),
    obspred   = map(pred, ~ tibble(pred = ., obs = y)),
    ROC       = map(obspred, pROC::roc, "obs", "pred", direction = "<"),
    AUCci     = map(ROC, pROC::ci.auc),
    AUC_lo    = map_dbl(AUCci, 1),
    AUC_est   = map_dbl(AUCci, 2),
    AUC_hi    = map_dbl(AUCci, 3),
    roc_auc   = map(obspred, roc_auc, obs, pred,   options = list(direction = ">")),
    roc_auc   = map_dbl(roc_auc, ".estimate"),
    roc_curve = map(obspred, roc_curve, obs, pred, options = list(direction = ">"))
  )

cache("all_models")


# Fig: ROC --------------------------------------------------------------------

fig_roc <-
  all_models %>%
  filter(fig) %>%
  unnest(roc_curve) %>%
  ggplot(aes(1 - specificity, sensitivity, col = Model)) +
  geom_path(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  theme_minimal() +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_blank()
  )

ggsave("graphs/brlasso_roc.png", fig_roc)


# Fig: AUC --------------------------------------------------------------------

fig_auc_ci <-
  all_models %>%
  mutate(Model = fct_reorder(Model, AUC_est)) %>%
  ggplot(aes(Model, AUC_est)) +
  geom_pointrange(aes(ymin = AUC_lo, ymax = AUC_hi), size = 1.5) +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  ylab("AUC with 95 % confidence intervals (De-Long)") +
  scale_y_continuous(breaks = seq(0, 1, .1))

ggsave("graphs/brlasso_auc_ci.png", fig_auc_ci)

# Fig: Separation ---------------------------------------------------------

fig_separation <-
  all_models %>%
  unnest(obspred) %>%
  filter(grepl("BRLasso|Age", Model), Model != "BRLasso all") %>%
  rename("Observed survival" = obs) %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(y = ..density.., fill = `Observed survival`),
    alpha = .3, position = "identity"
  ) +
  geom_density(aes(col = `Observed survival`)) +
  facet_wrap(
    ~ Model,
    #scales = "free",
    labeller = function(l) label_value(l, multi_line = FALSE)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30),
    axis.ticks = element_line(size = 1)
  ) +
  xlab("Predicted probability of death") +
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  scale_x_log10(limits = c(.0001, .1)) +
  expand_limits(x = 0)

ggsave("graphs/brlasso_separation_auc.png", fig_separation)


# Table: AUC ------------------------------------------------------------------

brlasso_tbl_auc <-
  all_models %>%
  arrange(desc(AUC_est)) %>%
  transmute(
    Model,
    AUC   = sprintf("%.2f (%.2f-%.2f)", AUC_est, AUC_lo, AUC_hi)
  )

cache("brlasso_tbl_auc")

# Table: model coefs ------------------------------------------------------

estimates <-
  all_models %>%
  filter(Model == "BRLasso 75%") %>%
  select(fit) %>%
  pluck(1, 1) %>%
  {left_join(glmCI(.), glmCI(., TRUE), "var", suffix = c(".beta", ".OR"))} %>%
  transmute(
    term = var,
    beta = sprintf("%.2f (%.2f-%.2f)", beta, low.beta, high.beta),
    OR   = sprintf("%.2f (%.2f-%.2f)", OR, low.OR, high.OR)
  )

p <-
  all_models %>%
  filter(Model == "BRLasso") %>%
  select(tidy) %>%
  pluck(1, 1) %>%
  transmute(
    term,
    p = pvalue_format()(p.value)
  )

brlasso_tbl_coefs <-
  left_join(estimates, p, "term") %>%
  mutate(
    term = gsub("[cP]_|_TRUE.|", "", term),
    term = gsub("_", " ", term),
  )

cache("brlasso_tbl_coefs")
