
# subModel <- function(Xpp, Y) {
#   #lm(Y~.,data.frame(Xpp, Y))
#   #step(lm(Y~.,data.frame(Xpp, Y)), trace = 0)
#   #svm(Y~.,data.frame(Xpp, Y))
#   randomForest(Y~.,data.frame(Xpp, Y), ntree = 400, mtry = 8)
# }


getPreproc <- function(Xtrain, Ytrain, Xtest, PCA = TRUE) {
  
  if (PCA == FALSE) {
    preproc <- function(X) {
      X
    }
  } else {
    
    if (PCA == TRUE) {
      nPCA <- 20 
    } else {
      stopifnot(is.integer(PCA))
      nPCA <- PCA
    }
    
    center.b <- FALSE
    
    wn.col <- grep(colnames(Xtrain), pattern = "m.*")
    #   wn.train <- rbind(Xtrain[,wn.col], Xtest[,wn.col])
    wn.train <- Xtrain[,wn.col]
    wn.pca <- prcomp(wn.train, center = center.b)
    pca.rot <- wn.pca$rotation
    pca.vecs <- wn.pca$x
    
    otherPredNames <- c("BSAN", "BSAS", "BSAV", "CTI", "ELEV", "EVI", "LSTD", 
                        "LSTN", "REF1", "REF2", "REF3", "REF7", "RELI", 
                        "TMAP", "TMFI", "Depth")
    #   matplot(pca.rot[,1:4], type = "l")
    
    preproc <- function(X) {
      if (center.b) {
        wn <- (data.matrix(X[,wn.col]) - (rep(1, nrow(X))  %o% wn.pca$center) )%*% pca.rot
      } else {
        wn <- data.matrix(X[,wn.col]) %*% pca.rot
      }
      otherPred <- X[, otherPredNames]
      return(data.frame(wn[,1:nPCA],otherPred))
    }
  }
  
  return(preproc)
}

fitModel <- function(subModel, Xt, Yt, preproc) {
  train <- function(Xt, Yt) {
    mod <- list()
    Xpp <- preproc(Xt)
    for (i in 1:ncol(Yt)) {
      mod[[i]] <- subModel(Xpp,Yt[,i])
    }
    return(mod)
  }
  
  predict.mod <- function(mod, Xh) {
    Y <- list()
    for (i in 1:length(mod)) {
      Y[[i]] <- predict(mod[[i]], data.frame(preproc(Xh)))
    }
    return(do.call(cbind,Y))
  }
  
  MOD <- train(Xt, Yt)
  
  function(Xtest) {
    predict.mod(MOD, Xtest)
  }
}

model <- function(Xtrain, Ytrain, Xtest) {
  preproc <- getPreproc(Xtrain, Ytrain, Xtest)
  totalPred <- fitModel(subModel, Xtrain, Ytrain, preproc)
  return(totalPred(Xtest))
}

getModelRF <- function(PCA = TRUE, ...) {
  library(randomForest)
  subModel <- function(Xpp, Y) {
    randomForest(Y~.,data.frame(Xpp, Y), ...)
  }
  
  function(Xtrain, Ytrain, Xtest) {
    preproc <- getPreproc(Xtrain, Ytrain, Xtest, PCA)
    totalPred <- fitModel(subModel, Xtrain, Ytrain, preproc)
    return(totalPred(Xtest))
  }
  
}

getModelSVM <- function(PCA = TRUE, ...) {
  library(e1071)
  subModel <- function(Xpp, Y) {
    svm(Y~.,data.frame(Xpp, Y), ...)
  }
  
  function(Xtrain, Ytrain, Xtest) {
    preproc <- getPreproc(Xtrain, Ytrain, Xtest, PCA)
    totalPred <- fitModel(subModel, Xtrain, Ytrain, preproc)
    return(totalPred(Xtest))
  }
  
}

getModelLinear <- function(PCA = TRUE, step.b = FALSE) {
  subModel <- function(Xpp, Y) {
    if( step.b ) {
      step(lm(Y~.,data.frame(Xpp, Y)), trace = 0)
    } else {
      lm(Y~.,data.frame(Xpp, Y))
    }
  }
  
  function(Xtrain, Ytrain, Xtest) {
    preproc <- getPreproc(Xtrain, Ytrain, Xtest, PCA)
    totalPred <- fitModel(subModel, Xtrain, Ytrain, preproc)
    return(totalPred(Xtest))
  }
  
}

model_zero <- function(Xtrain, Ytrain, Xtest) {
  tmp <- matrix(0, nrow = dim(Xtest)[1], ncol = 5)
  Ypred <- as.data.frame(tmp)
  names(Ypred) <- c("Ca", "P", "pH", "SOC", "Sand")
  return(Ypred)
}
