suppressMessages({library(ProjectTemplate); load.project()})

# Only for models with AUC CI lower > 0.7!

fig_separation <-
  all_models %>%
  select(Model, fig, obspred, AUC_lo) %>%
  unnest(obspred) %>%
  filter(fig, AUC_lo > .7) %>%
  rename("Observed survival" = obs) %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(y = ..density.., fill = `Observed survival`),
    alpha = .3, position = "identity"
  ) +
  geom_density(aes(col = `Observed survival`)) +
  # facet_wrap(
  #   ~ Model,
  #   #scales = "free",
  #   labeller = function(l) label_value(l, multi_line = FALSE)
  # ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30),
    axis.ticks = element_line(size = 1)
  ) +
  xlab("Predicted probability of death (log-scale)") +
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  scale_x_log10(limits = c(.0001, .1), labels = scales::percent) +
  expand_limits(x = 0)

ggsave("graphs/brlasso_separation_auc.png", fig_separation)
