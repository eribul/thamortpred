library(ProjectTemplate)
load.project()

# Nice text string of coefficients in full model
cfs       <- names(bestmodelcoefs)[-1]

cfs_dis   <-
  cfs[grepl("c_|Rx", cfs)] %>%
  {gsub("c_|Rx_|P_|TRUE|_Female|_X[123]", "", .)} %>%
  unique() %>%
  {gsub("_", " ", .)}

cfs_other <-
  cfs[!grepl("c_|Rx", cfs)] %>%
  {gsub("c_|Rx_|P_|TRUE|_Female|_X[123]", "", .)} %>%
  unique()


coefs_print <-
  paste0(
    paste(cfs_other, collapse = ", "),
    ", or the precense of ",
    glue::glue_collapse(cfs_dis, sep = ", ", last = " or "),
    ", during the year before surgery"
  )

cache("coefs_print")
