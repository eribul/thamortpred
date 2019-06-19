library(ProjectTemplate)
load.project()


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
