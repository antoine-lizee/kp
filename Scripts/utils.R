


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


CVMultiple <- function(models, X, Y, error = mcrmse, nFold = 3, ...) {
  
  cat("### Beginning CV...\n")
  N <- nrow(X)
  preproc <- getPreproc(X, ...)
  Xpp <- preproc(X)
  indexes <- sample(nFold, size = N, replace = T)
  
  perf <- array(dim = c(ncol(Y)+1, length(models)+1, nFold))
  
  for (iFold in 1:nFold) {
    Xtrain_i <- Xpp[indexes != iFold,]
    Ytrain_i <- Y[indexes != iFold,]
    Xtest_i <- Xpp[indexes == iFold,]
    Ytest_i <- Y[indexes == iFold,]
    
    Ytot_i <- array(dim = c(nrow(Xpp), ncol(Y), length(models)))
    
    ##Fit individual models
    for (iModel in 1:length(models)) {
      Ytot_ii <- models[[iModel]](Xtrain_i, Ytrain_i, rbind(Xtrain_i,Xtest_i))
      Ytot_i[,,iModel] <- Ytot_ii
      Ypred_i <- Ytot_ii[indexes == iFold,]
      perf[,iModel, iFold] <- error(Ytest_i, Ypred_i)
    }
    
    ##Fit ensemble model
    Ypred_em <- getModelRF(ntree = 100, mtry = 3)(Ytot_i[indexes != iFold,], Ytrain_i, Ytot_i[indexes == iFold,])
  }
  
  PERF = list(mean_cv_err = apply(perf, c(1,2), mean),
              std_cv_err = apply(perf, c(1,2), std))
  
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

