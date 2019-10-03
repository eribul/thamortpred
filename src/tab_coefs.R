suppressMessages({library(ProjectTemplate); load.project()})


# Table to present --------------------------------------------------------

brlasso_tbl_coefs <-
  all_models %>%
  filter(Model == "BRL any") %>%
  select(tidy) %>%
  pluck(1, 1) %>%
  transmute(
    term = gsub("[cP]_|TRUE", "", term),
    term = gsub("_", " ", term),
    X_text = c("", sprintf("X_%d", seq_len(n() - 1))),
    X_math = sprintf("$%s$", X_text) %>% {gsub("$$", "", ., fixed = TRUE)},
    beta = log(estimate),
    OR   = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p    = pvalue_format()(p.value)
  ) %>%
  mutate_at(vars(OR, p), ~ if_else(term == "(Intercept)", "", .))

cache("brlasso_tbl_coefs")


# List of coefficients for text -------------------------------------------


set_first <- function(coefs, x) {
  if (x %in% coefs) c(x, setdiff(coefs, x))
  else coefs
}

coefs_print <-
  brlasso_tbl_coefs %>%
  filter(term != "(Intercept)") %>%
  select(term) %>%
  pluck(1) %>%
  {gsub("GenderMan", "sex", .)} %>%
  {gsub("ECI ", "", .)} %>%
  {gsub("Age", "age", .)} %>%
  {gsub("cns", "CNS", .)} %>%
  {gsub("ASA[23]", "ASA", .)} %>%
  unique() %>%
  set_first("ASA") %>%
  set_first("sex") %>%
  set_first("age") %>%
  glue::glue_collapse(", ", last = " and ")


cache("coefs_print")



# Mathematical formula ----------------------------------------------------

coefs_form <-
  brlasso_tbl_coefs %>%
  transmute(
    coefs = ifelse(
      X_text == "",
      prettyNum(beta, digits = 2),
      sprintf("%.2f \\cdot %s", beta, X_text)
    )
  ) %>%
  summarise(coefs = paste(coefs, collapse = " + ")) %>%
  select(coefs) %>%
  pluck(1) %>%
  {sprintf("$$p = 1 / (1 +\\exp(%s))$$", .)}

cache("coefs_form")





# List variables in BRL all -----------------------------------------------


coefs_print_all <-
  all_models %>%
  filter(Model == "BRL all") %>%
  select(tidy) %>%
  pluck(1, 1) %>%
  transmute(
    term = gsub("[cP]_|TRUE", "", term),
    term = gsub("_", " ", term),
  ) %>%
  filter(term != "(Intercept)") %>%
  select(term) %>%
  pluck(1) %>%
  {gsub("cns", "CNS", .)} %>%
  {gsub("ASA[23]", "ASA", .)} %>%
  unique() %>%
  set_first("ASA") %>%
  set_first("sex") %>%
  set_first("age") %>%
  glue::glue_collapse(", ", last = " and ")

cache("coefs_print_all")
