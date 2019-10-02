# We will not consider any variable without observations in any combination
# of dead/alive-effected/not affected.
# This does not exclude any cases. It only exclude potential predictor variables

df0 <-
  df %>%
  select(
    -op_first, -op_last, -stime, -status,
    -contains("index"),
    -starts_with("death"), death90f
  )

y <- df0$death90f


# Find logical variables with empty cells for any combination ------------------

comb_lgl <-
  df0 %>%
  select_if(is.logical) %>%
  pivot_longer(everything()) %>%
  nest(data = c(value)) %>%
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
  pivot_wider(names_from = value_y, values_from = n)

cache("comb_lgl")

comb_lgl_text <-
  comb_lgl$name %>%
  {gsub("c_", "", .)} %>%
  {gsub("_", " ", .)} %>%
  {gsub("aids hiv", "AIDS/HIV", .)} %>%
  glue_collapse(", ", last = " and ")

cache("comb_lgl_text")

# Same for factor variables -----------------------------------------------

comb_fct_tmp <-
  df0 %>%
  select_if(is.factor) %>%
  select(-starts_with("death"))

n_lvls <-
  comb_fct_tmp %>%
  summarise_all(n_distinct) %>%
  pivot_longer(everything(), values_to = "n_lvls")

# Factor variables not affected
comb_fct <-
  comb_fct_tmp %>%
  pivot_longer(everything()) %>%
  nest(data = value) %>%
  left_join(n_lvls, "name") %>%
  mutate(
    data        = map(data, add_column, y),
    data        = map(data, count, y, value) #,
  ) %>%
  unnest(data) %>%
  pivot_wider(names_from = y, values_from = n) %>%
  mutate(
    name = if_else(duplicated(name), "", name)
  ) %>%
  select(-n_lvls)


# df without those variables ----------------------------------------------

df0 <-
  df0 %>%
  select(-one_of(comb_lgl$name), death90f) %>%
  mutate_if(is.logical, as.factor) # needed for recipe

cache("df0")

# Fix model matrix --------------------------------------------------------

df_model <-
  recipe(death90f ~., df0) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_predictors()) %>%
  prep(df0) %>%
  bake(df0)

cache("df_model")
