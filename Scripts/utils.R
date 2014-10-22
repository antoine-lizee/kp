


# Utility Functions -------------------------------------------------------

mcrmse <- function(Y,Y.hat){
  # Y and Y.hat have 6 columns (PIDN,Ca,P,pH,SOC,Sand)
  error <- numeric(0)
  for (i in 1:5){
    error <- c(error, sqrt(mean((Y[,i] - Y.hat[,i])^2)))
  }
  error <- c(error, mean(error))
  names(error) <- c("mseCa","mseP", "msepH", "mseSOC", "mseSand", "mcrmse")
  return(error)
}

CV <- function(model, X, Y, error = mcrmse, nFold = 3, ...) {
  
  cat("### Beginning CV...\n")
  N <- nrow(X)
  preproc <- getPreproc(X, ...)
  Xpp <- preproc(X)
  indexes <- sample(nFold, size = N, replace = T)
  
  perf <- list()
  
  for (iFold in 1:nFold) {
#     cat("## Iteration", iFold, "\n")
    Xtrain_i <- Xpp[indexes != iFold,]
    Ytrain_i <- Y[indexes != iFold,]
    Xtest_i <- Xpp[indexes == iFold,]
    Ytest_i <- Y[indexes == iFold,]
    Ypred_i <- model(Xtrain_i, Ytrain_i, Xtest_i)
    
    perf_i <- error(Ytest_i, Ypred_i)
#     print(perf_i)
    perf[[iFold]] <- perf_i
  }
  
  perf <- do.call(cbind, perf)
  
}


CVMultiple <- function(models, X, Y, error = mcrmse, nFold = 3,...) {
  
  cat("### Beginning CV...\n")
  N <- nrow(X)
  preproc <- getPreproc(X, ...)
  Xpp <- preproc(X)
  indexes <- sample(nFold, size = N, replace = T)
  
  nModels <- length(models)
  
  perf <- array(dim = c(ncol(Y)+1, nModels+1, nFold))
  
  for (iFold in 1:nFold) {
    Xtrain_i <- Xpp[indexes != iFold,]
    Ytrain_i <- Y[indexes != iFold,]
    Xtest_i <- Xpp[indexes == iFold,]
    Ytest_i <- Y[indexes == iFold,]
    
    Ytot_i <- array(dim = c(nrow(Xpp), ncol(Y), nModels))
    
    ##Fit individual models
    for (iModel in 1:nModels) {
      Ytot_ii <- models[[iModel]](Xtrain_i, Ytrain_i, Xpp)
      Ytot_i[,,iModel] <- Ytot_ii
      Ypred_i <- Ytot_ii[indexes == iFold,]
      perf[,iModel, iFold] <- error(Ytest_i, Ypred_i)
    }
    
    ##Fit ensemble model
    Ytot_i <- array(Ytot_i, dim = c(nrow(Xpp), ncol(Y) * length(models)))
    Ypred_em <- getModelRF(ntree = 100)(Ytot_i[indexes != iFold,], Ytrain_i, Ytot_i[indexes == iFold,])
    perf[,nModels+1, iFold] <- error(Ytest_i, Ypred_em)
    
  }
  
  PERF = list(mean_cv_err = apply(perf, c(1,2), mean),
              std_cv_err = apply(perf, c(1,2), sd))
  
}

CVMultiple2 <- function(models, X, Y, error = mcrmse, nFold = 3,...) {
  
  cat("### Beginning CV...\n")
  N <- nrow(X)
  preproc <- getPreproc(X, ...)
  Xpp <- preproc(X)
  indexes <- sample(nFold, size = N, replace = T)
  
  nModels <- length(models)
  
  perf <- array(dim = c(ncol(Y)+1, nModels+5, nFold))
  
  for (iFold in 1:nFold) {
    Xtrain_i <- Xpp[indexes != iFold,]
    Ytrain_i <- Y[indexes != iFold,]
    Xtest_i <- Xpp[indexes == iFold,]
    Ytest_i <- Y[indexes == iFold,]
    
    Ytot_i <- array(dim = c(nrow(Xpp), ncol(Y), nModels))
    
    ##Fit individual models
    for (iModel in 1:nModels) {
      Ytot_ii <- models[[iModel]](Xtrain_i, Ytrain_i, Xpp)
      Ytot_i[,,iModel] <- Ytot_ii
      Ypred_i <- Ytot_ii[indexes == iFold,]
      perf[,iModel, iFold] <- error(Ytest_i, Ypred_i)
    }
    
    ##Fit ensemble model
    Ytot_i <- array(Ytot_i, dim = c(nrow(Xpp), ncol(Y) * length(models)))
    Ypred_em <- getModelRF(ntree = 200)(Ytot_i[indexes != iFold,], Ytrain_i, Ytot_i[indexes == iFold,])
    perf[,nModels+1, iFold] <- error(Ytest_i, Ypred_em)
    Ypred_em <- getModelRF(ntree = 200, mtry = 0.2)(Ytot_i[indexes != iFold,], Ytrain_i, Ytot_i[indexes == iFold,])
    perf[,nModels+1, iFold] <- error(Ytest_i, Ypred_em)
    Ypred_em3 <- getModelGBM(n.trees = 2000, shrinkage = 0.005)(Ytot_i[indexes != iFold,], Ytrain_i, Ytot_i[indexes == iFold,])
    perf[,nModels+3, iFold] <- error(Ytest_i, Ypred_em3)
    Ypred_em3 <- getModelGBM(n.trees = 3000, shrinkage = 0.005)(Ytot_i[indexes != iFold,], Ytrain_i, Ytot_i[indexes == iFold,])
    perf[,nModels+4, iFold] <- error(Ytest_i, Ypred_em3)
    Ypred_em3 <- getModelGBM(n.trees = 5000, shrinkage = 0.005)(Ytot_i[indexes != iFold,], Ytrain_i, Ytot_i[indexes == iFold,])
    perf[,nModels+5, iFold] <- error(Ytest_i, Ypred_em3)
    
  }
  
  PERF = list(mean_cv_err = apply(perf, c(1,2), mean),
              std_cv_err = apply(perf, c(1,2), sd))
  
}


writeSubmission <- function(ids, Ypred, modelName){
  submission <- data.frame(PIDN = ids, Ca = Ypred[,1], 
                           P = Ypred[,2], pH = Ypred[,3], 
                           SOC = Ypred[,4], Sand = Ypred[,5])
  timeString <- gsub(Sys.time(), pattern = "\\s|:|PDT", replacement = "")
  write.csv(submission, paste0("Output/", timeString,"-", modelName ,"-submission.csv"),row.names = F)
}


# Test --------------------------------------------------------------------

if (test.b <- FALSE ) {
  Y <- read.csv("Output/sample_submission.csv")
  Y.hat <- read.csv("Output/submission.csv")
  print( mcrmse(Y[,-1], Y.hat[,-1]) )
  
  source("Scripts/init.R")
  source("Scripts/models.R")
  CV(model_zero, X = Xtot, Y =Ytot)
  
  writeSubmission(testdata$PIDN, model_zero(Xtot, Ytot, testdata), "M0")
}

