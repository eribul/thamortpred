suppressMessages({library(ProjectTemplate); load.project()})


preds <-
  bind_rows(

    baseline =
      tibble(
        c_cancer          = FALSE,
        c_cns             = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(1),
        ECI_obesity       = FALSE,
        P_Gender          = "Kvinna",
        P_Age             = min(df$P_Age),
        c_anemia          = FALSE,
        c_heart_condition = FALSE,
      ),

    healthy_female =
      tibble(
        c_cancer          = FALSE,
        c_cns             = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(1),
        ECI_obesity       = FALSE,
        P_Gender          = "Kvinna",
        P_Age             = 67,
        c_anemia          = FALSE,
        c_heart_condition = FALSE,
      ),

    sicker_man =
      tibble(
        c_cancer          = FALSE,
        c_cns             = FALSE,
        c_kidney_disease  = FALSE,
        P_ASA             = factor(3),
        ECI_obesity       = FALSE,
        P_Gender          = "Man",
        P_Age             = 78,
        c_anemia          = FALSE,
        c_heart_condition = TRUE,
      ),

    high_risk =
      tibble(
        c_cancer          = TRUE,
        c_cns             = TRUE,
        c_kidney_disease  = TRUE,
        P_ASA             = factor(3),
        ECI_obesity       = TRUE,
        P_Gender          = "Man",
        P_Age             = max(df$P_Age),
        c_anemia          = TRUE,
        c_heart_condition = TRUE,
      ),
    .id = "id"
  ) %>%
  mutate(
    p = sprintf("%s %%", prettyNum(predict(fit_brl_any, people, "response") * 100))
  )

cache("preds")
