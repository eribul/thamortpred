# General settings
memory.limit(1e10)
set.seed(132456798)
future::plan("multiprocess")
options('na.action' = "na.fail")

# Add any project specific configuration here.
add.config(
  apply.override = FALSE
)

# Add project specific configuration that can be overridden from load.project()
add.config(
  N_bots         = 10e3,
  apply.override = TRUE
)
