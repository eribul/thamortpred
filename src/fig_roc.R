suppressMessages({library(ProjectTemplate); load.project()})


# SHAR --------------------------------------------------------------------

shar_roc <-
  all_models %>%
  filter(fig) %>%
  unnest(roc_curve) %>%
  select(Model, specificity, sensitivity) %>%
  mutate(Model = model_names(Model))


# NJR ---------------------------------------------------------------------

njr_roc <-
  tibble(
    sensitivity = njr_ROC$sensitivities,
    specificity = njr_ROC$specificities
  ) %>%
  mutate(Model = "Reduced model (NJR)")


# Combine -----------------------------------------------------------------

all_roc <-
  bind_rows(
    shar_roc,
    bind_rows(
        njr_roc,
        filter(shar_roc, Model == "Reduced model")
      ),
    .id = "step"
  ) %>%
  mutate(step = factor(step, 1:2, c("Model derivation", "External validation")))


# Figure ------------------------------------------------------------------

fig_roc <-
  all_roc %>%
  ggplot(aes(1 - specificity, sensitivity, col = Model)) +
  geom_path(size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  theme_minimal() +
  theme(
    legend.position = "bottom", # c(1, 0),
    #legend.justification = c(1, 0),
    legend.title = element_blank()
  ) +
  facet_wrap(~ step) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)


ggsave("graphs/roc.png", fig_roc, height = 10, width = 15, units = "cm")
ggsave("graphs/roc.tiff", fig_roc, height = 10, width = 15, units = "cm", dpi = 1200, compression = "lzw")
