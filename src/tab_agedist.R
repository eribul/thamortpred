library(ProjectTemplate)
load.project()

# Tablef for age distribution

tab_agedist <-
df %>%
  mutate(
    Age = cut(P_Age, c(min(P_Age)-1, 65, 70, 75, 80, 85, 90, max(P_Age))),
    low = as.numeric(gsub("\\(([0-9]{2}),([0-9]{2})\\]", "\\1", Age)) + 1,
    hi = gsub("\\(([0-9]{2}),([0-9]{2})\\]", "\\2", Age),
    Age = sprintf("%d - %s", low, hi)
  ) %>%
  count(Age, death90f) %>%
  pivot_wider(names_from = death90f, values_from = n) %>%
  mutate(
    prop =
      sprintf(
        "%.2f %% (%d of %s)",
        dead / (dead + alive) * 100,
        dead,
        format(dead + alive, big.mark = ",")
      )
  ) %>%
  select(Age, prop)

cache("tab_agedist")

# Age range for patients who died
age_range <-
  df %>%
  filter(death90) %>%
  {range(.$P_Age)} %>%
  paste(collapse = "-")

cache("age_range")

# Median age for patient who died
age_median <-
  df %>%
  filter(death90) %>%
  {median(.$P_Age)}

cache("age_median")
