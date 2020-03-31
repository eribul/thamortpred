# Internal data connection used at the Center of Registers
# Not available and therefore not loaded in Binder
if (!grepl("thamortpred", Sys.info()[["nodename"]])) {
  con <- shar_linkage_con()
}

# General settings
utils::memory.limit(1e10)
set.seed(132456798)
future::plan("multiprocess")

options(
  digits    = 2,
  na.action = "na.fail",
  scipen    = 999
)

# Add any project specific configuration here.
add.config(
  apply.override = FALSE
)

# Add project specific configuration that can be overridden from load.project()
add.config(
  N_bots         = 100,
  Bmax           = 100,     # No of external reruns for BRLasso
  apply.override = TRUE
)
