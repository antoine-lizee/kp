


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

CV <- function(model, X, Y, error = mcrmse, nFold = 3) {
  
  cat("### Beginning CV...")
  N <- nrow(X)
  indexes <- sample(nFold, size = N, replace = T)
  
  perf <- list()
  
  for (iFold in 1:nFold) {
    cat("## Iteration", iFold, "\n")
    Xtrain_i <- X[indexes != iFold,]
    Ytrain_i <- Y[indexes != iFold,]
    Xtest_i <- X[indexes == iFold,]
    Ytest_i <- Y[indexes == iFold,]
    Ypred_i <- model(Xtrain_i, Ytrain_i, Xtest_i)
    
    perf_i <- error(Ytest_i, Ypred_i)
    print(perf_i)
    perf[[iFold]] <- perf_i
  }
  
  perf <- do.call(cbind, perf)
  
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

