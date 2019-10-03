suppressMessages({library(ProjectTemplate); load.project()})

# Simple plots from the rms-package
png("graphs/calibration.png", 1024, 1024)
  par(mfrow = c(3, 4))
  for (i in seq_len(nrow(all_models)))
    plot(all_models$lrm_calibrate[[i]], main = all_models$Model[[i]])
dev.off()

# Calibration belts with table and p-value from givitiR
png("graphs/calibration_belt.png", 1024, 1024)
  par(mfrow = c(3, 4))
  for (i in seq_len(nrow(all_models)))
    plot(
      all_models$cal_belt[[i]],
      main = all_models$Model[[i]],
      xlim = c(0, .15),
      ylim = c(0, .2)
    )
dev.off()

