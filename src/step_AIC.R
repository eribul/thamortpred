library(ProjectTemplate)
load.project()

# This is just a naive attempt to find a reasonably god model without validat ion,
# with only main effects, and simply based on AIC.
# Just for exploration!

df2 <-
  df %>%
  select(
    -stime,
    -status,
    -death1yr,
    -contains("index")
  )


# Evaluating different models ---------------------------------------------

fit_charlson <- glm(death90 ~ ., binomial, select(df2, -matches("elix|atc")))
fitstep_charlson <- step(fit_charlson)

fit_elix <- glm(death90 ~ ., binomial, select(df2, -matches("charlson|atc")))
fitstep_elix <- step(fit_elix)

fit_atc <- glm(death90 ~ ., binomial, select(df2, -matches("elix|charlson")))
fitstep_atc <- step(fit_atc)

cache("fitstep_charlson")
cache("fitstep_elix")
cache("fitstep_atc")


# Finding the best model --------------------------------------------------

# Take coefficients from models above, combine and step again

coefs <- function(mod) attr(attr(mod$model, "terms"), "term.labels")

rhs <-
  paste(unique(c(
    coefs(fitstep_atc),
    coefs(fitstep_charlson),
    coefs(fit_elix))),
    collapse = " + "
  )
f <- as.formula(paste("death90 ~", rhs))

fit <- glm(f, binomial, df2)
fit_best <- step(fit)
cache("fit_best")


# Best model with comorbidity indices -------------------------------------

# Take coefficients from above and add comorbidity indices
rhs <- paste(c(
  coefs(fit_best),
  names(df)[grepl("index", names(df))]),
  collapse = " + "
)

f <- as.formula(paste("death90 ~", rhs))
fit_super <- glm(f, binomial, df)
fitstep_super <- step(fit)

cache("fitstep_super")
