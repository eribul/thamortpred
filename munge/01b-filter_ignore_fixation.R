
filters_ignore_fixation <-
  filters %>%
  filter(is.na(excl) | !excl %in% c("Uncemented", "Hybrid", "Reverse hybrid"))

# Filter out cases row by row --------------------------------------------------
for (r in 2:nrow(filters_ignore_fixation)) {
  filters_ignore_fixation$data[[r]] <-
    filter(filters_ignore_fixation$data[[r - 1]], !!filters_ignore_fixation$expr[[r]])
}

df_shpr_ignore_fixation <- filters_ignore_fixation$data[[r]]
