# This file is supposed to run at Binder startup

if (grepl("thamortpred", Sys.info()[["nodename"]])) {
  dir.create("cache") # Must exist for ProjectTemplate to work
  library(ProjectTemplate)
  load.project()
  message(
    "This project is based on 'ProjectTemplate' (http://projecttemplate.net/).\n",
    "The project is pre-loaded (ProjectTemplate::load.project()).\n",
    "Enjoy!"
  )
}
