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

YPred <- list()

for (iModel in 1:nModels) {
  YPred[[iModel]] <- models[[iModel]](Xpp, Y, preproc(testdata))
}

YF <- cbind(YPred[[1]][,1], 
            YPred[[4]][,2],
            YPred[[1]][,3],
            YPred[[1]][,4],
            YPred[[1]][,5])

writeSubmission(ids = testdata$PIDN, Ypred = YF, "final_jcomprendspas.")

