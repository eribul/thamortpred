library(ProjectTemplate)
load.project()

config <- list(
  "introduce" = list(),
  "plot_histogram"   = list(),
  "plot_qq"          = list(),
  "plot_bar"         = list(),
  "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
  "plot_boxplot"     = list()
  )

DataExplorer::create_report(
  df, output_dir = "diagnostics", y = "death90f", config = config
)
