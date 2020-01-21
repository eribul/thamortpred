suppressMessages({library(ProjectTemplate); load.project()})

digs <- options("digits")
options(digits = 2)


njr_AUC <-
  tibble(
    Model   = "NJR (BRL reduced)",
    AUC_lo  = njr_AUCci[1],
    AUC_est = njr_AUCci[2],
    AUC_hi  = njr_AUCci[3]
  )

fig_auc_ci <-
  all_models %>%
  filter(fig) %>%
  select(Model, starts_with("AUC")) %>%
  bind_rows(njr_AUC) %>%
  pivot_longer(-Model) %>%
  filter(!is.na(value)) %>%
  separate(name, c("AUC","level", "corr")) %>%
  mutate(
    corr = case_when(
      startsWith(Model, "NJR") ~ "Validated",
      is.na(corr)              ~ "Estimated",
      TRUE                     ~ "Adjusted for optimism"
    )
  ) %>%
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
