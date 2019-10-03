suppressMessages({library(ProjectTemplate); load.project()})


tab_categorization <-
  categorization.Blad1 %>%
  mutate_all(zoo::na.locf) %>%
  group_by(new, from) %>%
  summarise(old = paste(old, collapse = ", ")) %>%
  pivot_wider(names_from = from, values_from = old)

cache("tab_categorization")
