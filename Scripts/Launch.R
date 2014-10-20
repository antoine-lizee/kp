


# Init --------------------------------------------------------------------
set.seed(27)
library(e1071)
source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

# linear Models -----------------------------------------------------------

PERF <- list()
# 
# (PERF[[1]] <- CV(getModelLinear(step.b = FALSE),Xtot,Ytot, nFold = 10))
# (PERF[[2]] <- CV(getModelLinear(step.b = TRUE),Xtot,Ytot, nFold = 10))
# PERF <- CV(getModelRF(ntree = 300, mtry = 8),Xtot,Ytot, nFold = 10)
# PERF<- c(PERF, list(CV(getModelRF(ntree = 300, mtry = 9),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelRF(ntree = 300, mtry = 10),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelRF(ntree = 300, mtry = 11),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelRF(ntree = 300, mtry = 12),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 10),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 100),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 1000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 10000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 50000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(kernel = "linear", cost = 1000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(kernel = "poly", cost = 1000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(kernel = "sigmoid", cost = 1000),Xtot,Ytot, nFold = 10)))

nFolds <- 4
PERF<- c(PERF, list(CV(getModelRF(PCA = FALSE, ntree = 300, mtry = 10),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = FALSE, ntree = 300, mtry = 15),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = FALSE, ntree = 300, mtry = 20),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = 20, ntree = 300, mtry = 10),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = 20, ntree = 300, mtry = 12),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = 20, ntree = 300, mtry = 14),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = 40, ntree = 300, mtry = 12),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = 40, ntree = 300, mtry = 14),Xtot,Ytot, nFolds)))
PERF<- c(PERF, list(CV(getModelRF(PCA = 40, ntree = 300, mtry = 16),Xtot,Ytot, nFolds)))

print(sapply(PERF, apply, 1, mean))
#writeSubmission(testdata$PIDN, model(Xtot, Ytot, testdata), "LM")

