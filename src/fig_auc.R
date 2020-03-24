suppressMessages({library(ProjectTemplate); load.project()})

digs <- options("digits")
options(digits = 2)


# NJR ---------------------------------------------------------------------

njr_AUC <-
  tibble(
    Model   = "Reduced model (NJR)",
    AUC_lo  = njr_AUCci[1],
    AUC_est = njr_AUCci[2],
    AUC_hi  = njr_AUCci[3]
  )



# Prepare data ------------------------------------------------------------

data_auc_ci <-
  all_models %>%
  filter(fig) %>%
  select(Model, starts_with("AUC")) %>%
  bind_rows(njr_AUC) %>%
  pivot_longer(-Model) %>%
  filter(!is.na(value)) %>%
  separate(name, c("AUC","level", "corr")) %>%
  mutate(
    corr = case_when(
      Model == "Reduced model (NJR)" ~ "NJR",
      is.na(corr)              ~ "SHAR",
      TRUE                     ~ "Adjusted for optimism"
    ),
    Model = model_names(Model)
  ) %>%
  pivot_wider(names_from = level) %>%
  mutate(Model = fct_reorder(Model, est)) %>%
  filter(corr %in% c("NJR", "SHAR"))

annotates <-
  data_auc_ci %>%
  pivot_longer(c(lo, est, hi))

# Figure ------------------------------------------------------------------

p <-
  ggplot(data_auc_ci, aes(Model, est, group = corr, color = corr)) +
  geom_hline(yintercept = .7, color = "darkgreen", linetype = "dashed", size = .5) +
  geom_pointrange(aes(ymin = lo, ymax = hi), size = .8) + #, position = position_dodge(.5)) +
  geom_text(aes(Model, value, label = round(value, 2)), data = annotates, nudge_x = .3, size = 2) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  ylab("AUC with 95% CI") +
  scale_y_continuous(breaks = seq(0, 1, .05))

options(digits = digs$digits)

ggsave("graphs/auc_ci.png", p, width = 10, height = 8, units = "cm")
ggsave("graphs/auc_ci.tiff", p, width = 10, height = 8, units = "cm", dpi = 1200, compression = "lzw")
