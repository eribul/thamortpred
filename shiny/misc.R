# file.copy("cache/fit_brl_reduced_lean.RData", "shiny/fit_brl_reduced_lean.RData", TRUE)
# file.copy("lib/clean_names.R", "shiny/clean_names_copy.R", TRUE)
load("fit_brl_reduced_lean.RData") # Absolute path correct for deployment
source("clean_names_copy.R")

coefs <-
  tibble(coef = names(coef(fit_brl_reduced_lean))[-1]) %>%
  mutate(
    checkbox     = grepl("TRUE", coef),
    coef_name    = gsub("(TRUE|Man|[23])$", "", coef),
    coef_present =
      clean_names(coef) %>%
      {gsub(" = [23]", "", .)} %>%
      {gsub("Obesity", "Diagnosed obesity", .)}

  ) %>%
  select(-coef) %>%
  distinct(coef_name, .keep_all = TRUE)

OR <- exp(coef(fit_brl_reduced_lean))

# Make tibble with patient data
pred <- function(input) {
  inp <- vector("list", nrow(coefs))
  names(inp) <- coefs$coef_name
  inp <- imap(inp, ~ if (.y %in% names(input)) input[[.y]])

  inp[coefs$coef_present %in% input$checkboxes] <- TRUE

  patdata <-
    map(inp, ~ if (is.null(.)) FALSE else .) %>%
    as_tibble(inp) %>%
    mutate(P_Gender = ifelse(P_Gender == "Male", "Man", "Kvinna"))

  (1 - predict(fit_brl_reduced_lean, patdata, "response")) * 100
}
