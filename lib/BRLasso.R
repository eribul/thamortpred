# Bootstrap ranking Lasso copied (and modified) from

BRLasso <- function(x, y, B = 5, Boots = 100, kfold = 10) {

  varx <- varx0 <- colnames(x)
  nvar          <- length(varx)
  n             <- length(y)
  var_list_all  <- vector("list", B)

  RecordM2 = matrix(0, nrow = B, ncol = nvar)
  rownames(RecordM2) <- paste0("B", seq_len(B))
  colnames(RecordM2) <- varx

  for (ij in seq_len(B)) {
    Nboot <- n
    RecordM = matrix(0, nrow = Boots, ncol = nvar)
    rownames(RecordM) = paste0("BSample", seq_len(Boots))
    colnames(RecordM) = varx
    for (i in seq_len(Boots)) {
      repeat {
        s <- sample(Nboot, replace = TRUE)
        if (length(table(y[s])) >= 2 & length(table(y[-s])) >= 2)
          break
      }

      cvfit <- glmnet::cv.glmnet(x[s,], y[s], nfolds = kfold, family = "binomial")
      model.final <- cvfit$glmnet.fit
      nzero       <- as.matrix(coef(model.final, s = cvfit$lambda.min))

      for (i0 in seq_len(dim(nzero)[1])) {
        for (j0 in seq_len(nvar)) {
          if (dimnames(nzero)[[1]][i0] == colnames(RecordM)[j0]) {
            RecordM[i, j0] = nzero[i0,]
          }
        }
      }
      message("Step 1-Current iteration: ", i)
    }

    Impor     <- abs(apply(RecordM, 2, mean))
    score     <- sort(Impor)
    index     <- seq_along(score)
    model.pwl <- SiZer::piecewise.linear(x = index, y = score)
    break_p   <- round(model.pwl$change.point)
    var_list_all[[ij]] <- names(score[break_p:length(index)])
    if (length(var_list_all[[ij]]) != 0) {
      varx0 <- intersect(var_list_all[[ij]], varx0)
    }
    Impor0    <- apply(RecordM, 2, mean)[var_list_all[[ij]]]
    for (gp in seq_along(Impor0)) {
      RecordM2[ij, colnames(RecordM2) == names(Impor0)[gp]] <- Impor0[gp]
    }
    message("   Step 2-Current iteration: ", ij)
  }
  if (length(varx0) == 1) {
    RecordM2 <- as.matrix(RecordM2[, varx0], ncol = 1)
    colnames(RecordM2) <- varx0
  } else {
    RecordM2 <- RecordM2[, varx0]
  }

  list(
    var.selected = colnames(RecordM2),
    var.coef = apply(RecordM2, 2, mean)
  )
}
