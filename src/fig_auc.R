suppressMessages({library(ProjectTemplate); load.project()})

digs <- options("digits")
options(digits = 2)

fig_auc_ci <-
  all_models %>%
  filter(fig) %>%
  select(Model, starts_with("AUC")) %>%
  pivot_longer(-Model) %>%
  separate(name, c("AUC","level", "corr")) %>%
  mutate(corr = ifelse(is.na(corr), "Estimated", "Adjusted for optimism")) %>%
  pivot_wider(names_from = level) %>%
  mutate(Model = fct_reorder(Model, est)) %>%

  ggplot(aes(Model, est, group = corr, color = corr)) +
  geom_hline(yintercept = .7, color = "darkgreen", linetype = "dashed", size = 2) +
  geom_pointrange(aes(ymin = lo, ymax = hi), size = 1.5, position = position_dodge(.5)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  ylab("AUC with 95% CI") +
  scale_y_continuous(breaks = seq(0, 1, .05))

options(digits = digs$digits)

ggsave("graphs/brlasso_auc_ci.png", fig_auc_ci)
