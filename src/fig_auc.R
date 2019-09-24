library(ProjectTemplate)
load.project()


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
