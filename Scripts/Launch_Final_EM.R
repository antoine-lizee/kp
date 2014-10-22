# Init --------------------------------------------------------------------
set.seed(27)
library(e1071)
source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

models <- list(getModelSVM(n_Features = 20, cost = 10, tolerance = 0.00001),
               getModelSVM(n_Features = 15, cost = 10, tolerance = 0.00001),
               getModelSVM(n_Features = 25, cost = 10, tolerance = 0.00001),
               getModelSVM(n_Features = 80, cost = 10, tolerance = 0.00001),
               getModelSVM(n_Features = 256, cost = 1000, tolerance = 0.00001),
               getModelRF(n_Features = 15, ntree = 200, mtry = 8),
               getModelRF(n_Features = 40, ntree = 200, mtry = 14))

preproc <- getPreproc(Xtot)
Xpp <- preproc(Xtot)
Y <- Ytot

nModels <- length(models)


Ytot_i <- array(dim = c(nrow(Xpp)+nrow(testdata), ncol(Y), nModels))

for (iModel in 1:nModels) {
  Ytot_ii <- models[[iModel]](Xpp, Y, preproc(rbind(Xtot, testdata)))
  Ytot_i[,,iModel] <- Ytot_ii
  Ypred_i <- Ytot_ii[1:nrow(Xtot),]
}

Ytot_i <- array(Ytot_i, dim = c(nrow(Xpp)+nrow(testdata), ncol(Y) * length(models)))
# Ypred_em <- getModelRF(ntree = 200)(Ytot_i[1:nrow(Xtot),], Ytot, Ytot_i[nrow(Xtot) + 1:nrow(testdata),])
Ypred_em <- getModelGBM(n.trees = 3000, shrinkage = 0.005)(Ytot_i[1:nrow(Xtot),], Ytot, Ytot_i[nrow(Xtot) + 1:nrow(testdata),])


writeSubmission(ids = testdata$PIDN, Ypred = Ypred_em, "final_RFEMGBM")

## The main problem was overfitting, mainly because of:
# - the non-equivalence of the train/test data
# - the high variablility of some results compared to the small amount of data
# --> bootstraping techniques ??
# --> better loss functions ??