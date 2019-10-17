suppressMessages({library(ProjectTemplate); load.project()})

all_models_fig <- all_models #filter(all_models, fig)

# Simple plots from the rms-package
png("graphs/tmp_calibration.png", 1024, 1024)
  par(mfrow = c(3, 4))
  for (i in seq_len(nrow(all_models_fig))) {
    plot(
      all_models_fig$lrm_calibrate[[i]],
        main = all_models_fig$Model[[i]],
        # xlim = c(0, 0.03),
        # ylim = c(0, 0.03)
    )
  }
dev.off()

# Calibration belts with table and p-value from givitiR
png("graphs/tmp_calibration_belt.png", 1024, 1024)
  par(mfrow = c(3, 4))
  for (i in seq_len(nrow(all_models_fig))) {
    plot(
      all_models_fig$cal_belt[[i]],
      main = all_models_fig$Model[[i]],
      xlim = c(0, .1),
      ylim = c(0, .1)
    )
  }
dev.off()



# BRL 33 % model ----------------------------------------------------------

tcks <- seq(.0, .1, .01)

png("graphs/calibration_belt.png", 1024, 1024, pointsize = 36)
  all_models %>%
    filter(Model == "BRL reduced (age as main effect)") %>% {
      plot(
        pluck(.$cal_belt, 1),
        xlim = c(0, .06),
        ylim = c(0, .08),
        xlab = "Predicted probabilities [%]",
        ylab = "Observed  probabilities [%]",
        main = NULL,
        table = FALSE,
        polynomialString = FALSE,
        pvalueString = FALSE,
        nString = FALSE,
        mar = c(5, 4, 0, 0) + 0.1,
        xaxt = "n",
        yaxt = "n",

      )
      abline(v = .03, lty = "dashed", col = "darkgreen", lwd = 3)
      axis(1, at = tcks, lab = sprintf("%.0f", tcks * 100), las = TRUE)
      axis(2, at = tcks, lab = sprintf("%.0f", tcks * 100), las = TRUE)
    }
dev.off()

