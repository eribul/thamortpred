# Vi tyckte det var kul att j�mf�ra med en enkel logistisk regressionsmodell.
# Denna b�r naturligtvis bli bet s�mre vid extern validering.

library(ProjectTemplate)
load.project()

logreg <- glm(death90f ~ ., binomial, df_model)
p <- predict(logreg, type = "response")

obspred <- tibble(obs = df_model$death90f, pred = predict(logreg, type = "response"))

pROC::roc(obspred, obs, pred, levels = c("alive", "dead"), direction = "<", auc = TRUE, ci = TRUE)
# Area under the curve: 0.79
# 95% CI: 0.76-0.83 (DeLong)
