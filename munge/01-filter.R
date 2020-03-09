# library(tidyverse)
# library(DiagrammeR)
# library(DiagrammeRsvg)


# Define filter steps -----------------------------------------------------

# Make tibble with one row per filter step and columns:
# step: Number identifying inclusion box.
#       Does not need to be unique if several exlusions are combined.
# incl: Description of included cases
# excl: Description of cases to exclude
# expr: quosure defining filter step based on variable names from the data
# data: The dataset to be filtered. Only required for initial step (first row).

filters <-
  bind_rows(
    tibble(
      step = 0,
      incl = "all",
      data = list(df_shpr_orig)
    ),

    tibble(
      step = 1,
      excl = "Hemi- and surface prosthesis",
      incl = "THA 2008-2015",
      expr = list(quo(
        P_ProstType == "Totalprotes" &
        is.na(P_KVA1) |
          P_KVA1 != "NFB62 - Primär total ytersättningspr"
      ))
    ),

    tibble(
      step = 2,
      excl = "Other diagnoses than OA",
      incl = "THA due to OA",
      expr = list(quo(P_DiaGrp == "Primär artros"))
    ),

    tibble(
      step = 3,
      excl = "First THA for bilateral cases",
      incl = "Patients with OA",
      expr = list(quo(op_last == 1))
    ),

    tibble(
      step = 4,
      excl = "Uncemented",
      incl = "Patients with OA\\l and cemented THA",
      expr = list(quo(P_FemStemCemMix != "Cementfritt" |
                      P_AcetCupCemMix != "Cementfritt"))
    ),

    tibble(
      step = 4,
      excl = "Hybrid",
      incl = "Patients with OA\\l and cemented THA",
      expr = list(quo(P_AcetCupCemMix != "Cementfritt"))
    ),

    tibble(
      step = 4,
      excl = "Reverse hybrid",
      incl = "Patients with OA\\l and cemented THA",
      expr = list(quo(P_FemStemCemMix != "Cementfritt"))
    ),

    tibble(
      step = 5,
      excl = "Age < 18 or > 100",
      incl = "Total study population",
      expr = list(quo(between(P_Age, 18, 100)))
    ),

    tibble(
      step = 5,
      excl = "BMI missing",
      incl = "Total study population",
      expr = list(quo(!is.na(P_BMI)))
    ),

    tibble(
      step = 5,
      excl = "BMI > 50",
      incl = "Total study population",
      expr = list(quo(P_BMI <= 50))
    ),

    tibble(
      step = 5,
      excl = "ASA missing",
      incl = "Total study population",
      expr = list(quo(!is.na(P_ASA)))
    ),

    tibble(
      step = 5,
      excl = "ASA class IV-V",
      incl = "Total study population",
      expr = list(quo(P_ASA <= 3))
    ),

    tibble(
      step = 5,
      excl = "Missing education",
      incl = "Total study population",
      expr = list(quo(!is.na(education)))
    ),

    tibble(
      step = 5,
      excl = "Missing civil status",
      incl = "Total study population",
      expr = list(quo(!is.na(civil_status)))
    ),

    tibble(
      step = 5,
      excl = "Missing type of hospital",
      incl = "Training sample",
      expr = list(quo(!is.na(P_TypeOfHospital)))
    )

  )


# Filter out cases row by row --------------------------------------------------
for (r in 2:nrow(filters)) {
  filters$data[[r]] <- filter(filters$data[[r - 1]], !!filters$expr[[r]])
}

# Save final data set as df
df_shpr <- filters$data[[r]]
cache("df_shpr")


# Format table with flowchart data ---------------------------------------------

filters <-
  filters %>%
  mutate(
    N        = map_int(data, ~ n_distinct(.$LopNr)),
    N_s      = prettyNum(N, big.mark = ",", preserve.width = "none"),
    N_excl   = lag(N) - N,
    N_excl_s = prettyNum(N_excl, big.mark = ",", preserve.width = "none")
  ) %>%
  filter(N_excl > 0) %>%
  group_by(step) %>%
  mutate(
    excl_text =
      if (n() == 1) {
        sprintf("%s\\l(N = %s)\\l", excl, N_excl_s)
      } else {
        sprintf(
          "Exclusion of (N = %s):\\l  - %s\\l",
          format(sum(N_excl), big.mark = ","),
          paste(sprintf("%s (N = %s)", excl, N_excl_s), collapse = "\\l  - ")
        )
      },
    incl_text  = sprintf("%s\\l (N = %s)\\l", incl, N_s)
  ) %>%
  ungroup() %>%
  mutate(excl_next = lead(excl_text)) %>%
  select(step, incl_text, excl_next) %>%
  distinct(excl_next, .keep_all = TRUE)


# Make graph --------------------------------------------------------------

flowchart_shar <- flowchart(filters)
cache("flowchart_shar")
