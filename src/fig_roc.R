suppressMessages({library(ProjectTemplate); load.project()})


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
