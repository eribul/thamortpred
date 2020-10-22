# Vi tyckte det var kul att jämföra med en enkel logistisk regressionsmodell.
# Denna bör naturligtvis bli bet sämre vid extern validering.

library(ProjectTemplate)
load.project()

logreg <- glm(death90f ~ ., binomial, df_model)
p <- predict(logreg, type = "response")

obspred <- tibble(obs = df_model$death90f, pred = predict(logreg, type = "response"))

pROC::roc(obspred, obs, pred, levels = c("alive", "dead"), direction = "<", auc = TRUE, ci = TRUE)
# Area under the curve: 0.79
# 95% CI: 0.76-0.83 (DeLong)
