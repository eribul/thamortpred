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
        P_Age             = min(df$P_Age),
        c_anemia          = FALSE,
        c_heart_condition = FALSE,
      ),

    healthy_female =
      tibble(
        c_cancer          = FALSE,
        c_cns_disease     = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(1),
        c_obesity         = FALSE,
        P_Gender          = "Kvinna",
        P_Age             = 67,
        c_anemia          = FALSE,
        c_heart_condition = FALSE,
      ),

    sicker_man =
      tibble(
        c_cancer          = FALSE,
        c_cns_disease     = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(3),
        c_obesity         = FALSE,
        P_Gender          = "Man",
        P_Age             = 78,
        c_anemia          = FALSE,
        c_heart_condition = TRUE,
      ),

    sickest_man =
      tibble(
        c_cancer          = TRUE,
        c_cns_disease     = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(3),
        c_obesity         = FALSE,
        P_Gender          = "Man",
        P_Age             = max(df$P_Age),
        c_anemia          = FALSE,
        c_heart_condition = TRUE,
      ),
    .id = "id"
  ) %>%
  mutate(
    p = sprintf("%s %%", prettyNum(predict(fit_brl_reduced$glm, ., "response") * 100))
  )

cache("people")
