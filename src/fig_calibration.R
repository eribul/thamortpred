suppressMessages({library(ProjectTemplate); load.project()})

# SHAR (internal calibration) ---------------------------------------------

shar_calibration <-
  all_models %>%
  filter(Model == "BRL reduced (age as main effect)") %>%
  select(cal_belt) %>%
  pluck(1, 1)


# Extract relevant data ---------------------------------------------------

beltplot2tibble <- function(cb) {
  tibble(
    x = cb$seqP,
    L = cb$cbBoundByConfLevel[[1]]$L,
    U = cb$cbBoundByConfLevel[[1]]$U
  )
}

cbs <-
  bind_rows(
    `External calibration (NJR)`  = beltplot2tibble(njr_calibration3),
    `Internal calibration (SHAR)` = beltplot2tibble(shar_calibration),
    .id = "validation"
  )

# Make figure -------------------------------------------------------------

ggplot(cbs, aes(x, ymin = L, ymax = U, fill = validation)) +
  geom_ribbon(alpha = .3) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format(1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(.01, .06, 0.01), minor_breaks = NULL, labels = scales::percent_format(1)) +
  coord_cartesian(c(0, 0.05), c(0, 0.065)) +
  xlab("Predicted probability") +
  ylab("Observed proportion") +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_blank()
  )

ggsave("graphs/calibration.png", height = 10, width = 10 , units = "cm")
