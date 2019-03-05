library(ProjectTemplate)
load.project()

config <- list(
  "introduce" = list(),
  # "plot_str"   = list(
  #   "type"     = "diagonal",
  #   "fontSize" = 35,
  #   "width"    = 1000,
  #   "margin"   = list("left" = 350, "right" = 250)
  # ),
  #"plot_missing"     = list(),
  "plot_histogram"   = list(),
  "plot_qq"          = list(),
  "plot_bar"         = list(),
  "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
  # "plot_prcomp"    = list(),
  "plot_boxplot"     = list()#,
  # "plot_scatterplot" = list()
)

DataExplorer::create_report(
  df, output_dir = "diagnostics", y = "death90f", config = config
)