library(ProjectTemplate)
load.project()

# This is just to calculate over-all survival as background info

fit <- survfit(Surv(stime, status) ~ 1, df)

summary(fit, c(c(30, 90) / 365.241, 1:5), scale = 365.241)

survminer::ggsurvplot(
  fit,
  risk.table = TRUE,
  censor = FALSE,
  ylim = c(.7, 1),
  surv.scale = "percent",
  xscale = "d_y",
  break.time.by = 365.241
)

ggsave("graphs/km.png")
