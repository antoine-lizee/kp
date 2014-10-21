


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
# PERF<- c(PERF, list(CV(c,Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 100),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 1000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 10000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(cost = 50000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(kernel = "linear", cost = 1000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(kernel = "poly", cost = 1000),Xtot,Ytot, nFold = 10)))
# PERF<- c(PERF, list(CV(getModelSVM(kernel = "sigmoid", cost = 1000),Xtot,Ytot, nFold = 10)))


#PERF<- c(PERF, list(CV(getModelSVM(PCA = 40, cost = 10),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = 40, cost = 500),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = 40, cost = 10000),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = 40, cost = 100000),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = 80, cost = 10),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = 80, cost = 500),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = 80, cost = 10000),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = FALSE, cost = 100000),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = FALSE, cost = 10),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = FALSE, cost = 500),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = FALSE, cost = 10000),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelSVM(PCA = FALSE, cost = 100000),Xtot,Ytot, nFold = nFolds)))


#PERF<- c(PERF, list(CV(getModelRF(PCA = 40, ntree = 300, mtry = 12),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelRF(PCA = 40, ntree = 300, mtry = 14),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelRF(PCA = 40, ntree = 300, mtry = 16),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelRF(PCA = 60, ntree = 300, mtry = 12),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelRF(PCA = 60, ntree = 300, mtry = 14),Xtot,Ytot, nFold = nFolds)))
#PERF<- c(PERF, list(CV(getModelRF(PCA = 60, ntree = 300, mtry = 16),Xtot,Ytot, nFold = nFolds)))

# 
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 10, cost = 5),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 20, cost = 5),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 30, cost = 5),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 40, cost = 5),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 10, cost = 10),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 20, cost = 10),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 30, cost = 10),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 40, cost = 10),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 10, cost = 20),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 20, cost = 20),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 30, cost = 20),Xtot,Ytot, nFold = nFolds)))
# PERF<- c(PERF, list(CV(getModelSVM(PCA = 40, cost = 20),Xtot,Ytot, nFold = nFolds)))


# print(t(sapply(PERF, apply, 1, mean)))
#writeSubmission(testdata$PIDN, getModelSVM(PCA = 40, cost = 10000)(Xtot, Ytot, testdata), "SVM-PCA40-Cost10")



# Mutliple CV -------------------------------------------------------------

PERF2 <- CVMultiple(list(getModelSVM(cost = 10000),
                         getModelSVM(cost = 10),
                         getModelRF(ntree = 200, mtry = 12)),
                    Xtot,
                    Ytot,
                    nFold = 3,
                    PCA = 20)
