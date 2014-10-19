


# Utility Functions -------------------------------------------------------

mcrmse <- function(Y,Y.hat){
  # Y and Y.hat have 6 columns (PIDN,Ca,P,pH,SOC,Sand)
  error <- numeric(0)
  for (i in 1:5){
    error <- c(error, sqrt(mean((Y[i] - Y.hat[i])^2)))
  }
  error <- c(error, mean(error))
  names(error) <- c("mseCa","mseP", "msepH", "mseSOC", "mseSand", "mcrmse")
  return(error)
}

CV <- function(model, X, Y, error, nFold = 3) {
  
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
    perf[[i]] <- perf_i
  }
  
  perf <- do.call(cbind, perf)
  
}


# Test --------------------------------------------------------------------

if (test.b <- TRUE) {
  Y <- read.csv("Output/sample_submission.csv")
  Y.hat <- read.csv("Output/submission.csv")
  
  print( mcrmse(Y, Y.hat) )
  
  
}

