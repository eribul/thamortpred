suppressMessages({library(ProjectTemplate); load.project()})

obspred <-
  all_models %>%
  select(Model, fig, obspred, AUC_lo) %>%
  unnest(obspred) %>%
  filter(Model == "BRL reduced (age as main effect)") %>%
  mutate(
    "Observed survival" =
      factor(obs, c("dead", "alive"),
                  c("Died within 90 days", "Survived at least 90 days")
      )
  )

pred_probs <- obspred$pred * 100
cache("pred_probs")



# Histogram ---------------------------------------------------------------

fig_separation_hist <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(
      # y = ..density..,
      fill = `Observed survival`
    ),
    alpha = .5, position = "identity"
  ) +
  geom_vline(aes(xintercept = .05), color = "darkgreen", linetype = "dashed") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 30),
    axis.ticks      = element_line(size = 1),
    axis.line       = element_line(size = .25),
    axis.ticks.length.x.bottom = unit(.25, "cm"),
    axis.ticks.x = element_line(size = 0.25),
  ) +
  xlab(expression(paste("Predicted probability of death [", log[10], "]"))) +
  ylab(expression(paste(sqrt(n)))) +
  scale_x_log10(
    breaks = c(.01, .1, 1, 3, 6, 10) / 100,
    limits = c(.0001, .1),
    labels = scales::percent
  ) +
  scale_y_sqrt(
    breaks = c(0, 100, 500, 1000, 2000, 3000, 4000, 5000), #seq(0, 6000, 1000),
    labels = function(x) format(x, big.mark = ",")
  ) +
  expand_limits(x = 0)


# Density -----------------------------------------------------------------

fig_separation_density <-
  obspred %>%
  ggplot(aes(pred)) +
  geom_histogram(
    aes(
      y = ..density..,
      fill = `Observed survival`
    ),
    alpha = .5, position = "identity"
  ) +
  geom_density(aes(col = `Observed survival`)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 30),
    axis.ticks.length.x.bottom = unit(.25, "cm"),
    axis.ticks.x = element_line(size = 0.25),
    axis.ticks      = element_line(size = 1),
    axis.ticks.y    = element_line(color = "white"),
    axis.text.y     = element_text(color = "white", size = 17),     # To align with upper panel
    axis.line       = element_line(size = .25)
  ) +
  geom_vline(aes(xintercept = .05), color = "darkgreen", linetype = "dashed") +
  xlab(expression(paste("Predicted probability of death [", log[10], "]"))) +
  ylab("Density") +
  scale_x_log10(
    breaks = c(.01, .1, 1, 3, 6, 10) / 100,
    limits = c(.0001, .1),
    labels = scales::percent
  ) +
  expand_limits(x = 0)


# Combine plots -----------------------------------------------------------

figs <- gridExtra::grid.arrange(fig_separation_hist, fig_separation_density, nrow = 2)

ggsave("graphs/separation.png", figs, height = 15, width = 12, units = "cm")
ggsave("graphs/separation.tiff", figs, height = 15, width = 12, units = "cm", dpi = 1200, compression = "lzw")


# Separate figs for BJJ
ggsave("graphs/separation_hist.tiff", fig_separation_hist, height = 10, width = 10, units = "cm", dpi = 1200, compression = "lzw")
ggsave("graphs/separation_dens.tiff", fig_separation_density, height = 10, width = 10, units = "cm", dpi = 1200, compression = "lzw")
