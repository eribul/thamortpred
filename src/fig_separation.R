library(ProjectTemplate)
load.project()


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
