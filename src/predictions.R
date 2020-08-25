suppressMessages({library(ProjectTemplate); load.project()})


people <-
  bind_rows(

    baseline =
      tibble(
        c_cancer          = FALSE,
        c_cns_disease     = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(1),
        c_obesity         = FALSE,
        P_Gender          = "Kvinna",
        P_Age             = 57
      ),

    healthy_female =
      tibble(
        c_cancer          = FALSE,
        c_cns_disease     = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(1),
        c_obesity         = FALSE,
        P_Gender          = "Kvinna",
        P_Age             = 67
      ),

    sicker_man =
      tibble(
        c_cancer          = FALSE,
        c_cns_disease     = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(3),
        c_obesity         = FALSE,
        P_Gender          = "Man",
        P_Age             = 78
      ),

    sickest_man =
      tibble(
        c_cancer          = TRUE,
        c_cns_disease     = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(3),
        c_obesity         = FALSE,
        P_Gender          = "Man",
        P_Age             = 95
      ),
    .id = "id"
  ) %>%
  mutate(
    p      = predict(fit_brl_reduced$glm, ., "response"),
    p_perc = sprintf("%s %%", prettyNum(p * 100)),
    p_prom = sprintf("%s \u2030", prettyNum(p * 1000)),
    p      = ifelse(p < .01, p_prom, p_perc)
  )

cache("people")
