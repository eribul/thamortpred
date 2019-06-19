library(ProjectTemplate)
load.project()

# Make data set for plotting
obspreds <-
  rocs %>%
  unnest(tr_obspred)

# Difference in estimated probabilities for dead and alive
# Note that y-axis missleading due to downsampling!
obspreds %>%
  rename("Observed survival" = obs) %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(y = ..density.., fill = `Observed survival`),
    alpha = .3, position = "identity"
  ) +
  geom_density(aes(col = `Observed survival`)) +
  facet_wrap(~ name, scales = "free",
             labeller = function(l) label_value(l, multi_line = FALSE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("Predicted probability of death") +
  ylab("") +
  scale_y_continuous(breaks = NULL)

ggsave("graphs/separation_auc.png")
