library(ProjectTemplate)
load.project()

# AUC plot
aucs %>%
  mutate(Model = fct_reorder(name, AUCtrain_est)) %>%
  ggplot(aes(Model, AUCtrain_est)) +
  geom_hline(aes(yintercept = .5), linetype = 2, col = "grey", size = 2) +
  geom_pointrange(aes(ymin = AUCtrain_lo, ymax = AUCtrain_hi), size = 1.5) +
  coord_flip() +
  theme_minimal() +
  ylab("AUC") +
  scale_y_continuous(breaks = seq(0, 1, .1))

ggsave("graphs/auc_ci.png")
