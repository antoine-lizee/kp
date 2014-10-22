

#subModel <- function(Xpp, Y) {
#   #lm(Y~.,data.frame(Xpp, Y))
#   #step(lm(Y~.,data.frame(Xpp, Y)), trace = 0)
#   svm(Y~.,data.frame(Xpp, Y))
#   randomForest(Y~.,data.frame(Xpp, Y), ntree = 400, mtry = 8)
#}


getPreproc <- function(Xtrain, PCA = TRUE) {
  # returns the preprocessing function
  if (PCA == FALSE) {
    preproc <- function(X) {
      X
    }
  } else {
    
    if (PCA == TRUE) {
      nPCA <- min(nrow(Xtrain), ncol(Xtrain))
    } else {
      stopifnot(is.numeric(PCA))
      nPCA <- PCA
    }
    
    center.b <- TRUE
    
    wn.col <- grep(colnames(Xtrain), pattern = "m.*")
    #i.remove <- which(colnames(Xtrain) == "m2379.76")
    #j.remove <- which(colnames(Xtrain) == "m2352.76")
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

fitModel <- function(subModel, Xt, Yt, ...) {
  train <- function(Xt, Yt) {
    mod <- list()
    #Xpp <- preproc(Xt)
    Xpp <- Xt
    for (i in 1:ncol(Yt)) {
      mod[[i]] <- subModel(Xpp,Yt[,i])
    }
    return(mod)
  }
  
  predict.mod <- function(mod, Xh) {
    Y <- list()
    for (i in 1:length(mod)) {
      Y[[i]] <- predict(mod[[i]], data.frame(Xh), ...)
    }
    return(do.call(cbind,Y))
  }
  
  MOD <- train(Xt, Yt)
  
  function(Xtest) {
    predict.mod(MOD, Xtest)
  }
}


model <- function(Xtrain, Ytrain, Xtest) {
  #preproc <- getPreproc(Xtrain)
  totalPred <- fitModel(subModel, Xtrain, Ytrain)
  return(totalPred(Xtest))
}

getModelRF <- function(n_Features = NULL, ...) {
  library(randomForest)
  subModel <- function(Xpp, Y) {
    randomForest(Y~.,data.frame(Xpp, Y), ...)
  }
  
  function(Xtrain, Ytrain, Xtest) {
    if (!is.null(n_Features)) {
      Xtrain <- Xtrain[, 1:n_Features]
      Xtest <- Xtest[, 1:n_Features]
    }
    totalPred <- fitModel(subModel, Xtrain, Ytrain)
    return(totalPred(Xtest))
  }
  
}

getModelSVM <- function(n_Features = NULL, ...) {
  library(e1071)
  subModel <- function(Xpp, Y) {
    svm(Y~.,data.frame(Xpp, Y), ...)
  }
  
  function(Xtrain, Ytrain, Xtest) {
    if (!is.null(n_Features)) {
      Xtrain <- Xtrain[, 1:n_Features]
      Xtest <- Xtest[, 1:n_Features]
    }
    totalPred <- fitModel(subModel, Xtrain, Ytrain)
    return(totalPred(Xtest))
  }
  
}


getModelLinear <- function(n_Features = NULL, step.b = FALSE) {
  subModel <- function(Xpp, Y) {
    if( step.b ) {
      step(lm(Y~.,data.frame(Xpp, Y)), trace = 0)
    } else {
      lm(Y~.,data.frame(Xpp, Y))
    }
  }
  
  function(Xtrain, Ytrain, Xtest) {
    totalPred <- fitModel(subModel, Xtrain, Ytrain)
    return(totalPred(Xtest))
  }
  
}

getModelGBM <- function(n_Features = NULL, n.trees, ...) {
  library(gbm)
  subModel <- function(Xpp, Y) {
    gbm(Y~., data.frame(Xpp, Y), distribution = "gaussian", n.trees = n.trees, ...)
  }
  
  function(Xtrain, Ytrain, Xtest) {
    if (!is.null(n_Features)) {
      Xtrain <- Xtrain[, 1:n_Features]
      Xtest <- Xtest[, 1:n_Features]
    }
    totalPred <- fitModel(subModel, Xtrain, Ytrain, n.trees = n.trees)
    return(totalPred(Xtest))
  } 
}


model_zero <- function(Xtrain, Ytrain, Xtest) {
  tmp <- matrix(0, nrow = dim(Xtest)[1], ncol = 5)
  Ypred <- as.data.frame(tmp)
  names(Ypred) <- c("Ca", "P", "pH", "SOC", "Sand")
  return(Ypred)
}

fitModels <- function(subModels, Xt, Yt, n_Features) {
  train <- function(Xt, Yt) {
    mod <- list()
    Xpp <- Xt
    for (i in 1:ncol(Yt)) {
      if (!is.null(n_Features[[i]])) {
        Xpp <- Xpp[, 1:n_Features[[i]]]
      }
      mod[[i]] <- subModels[[i]](Xpp,Yt[,i])
    }
    return(mod)
  }
  
  predict.mod <- function(mod, Xh) {
    Y <- list()
    for (i in 1:length(mod)) {
      if (!is.null(n_Features[[i]])) {
        Xh <- Xh[, 1:n_Features[[i]]]
      }
      Y[[i]] <- predict(mod[[i]], data.frame(Xh))
    }
    return(do.call(cbind,Y))
  }
  
  MOD <- train(Xt, Yt)
  
  function(Xtest) {
    predict.mod(MOD, Xtest)
  }
}


getModelSVMTot <- function(n_Features, costs, ...) {
  library(e1071)
  subModels <- lapply(costs, function(cost) {
    function(Xpp, Y) {
      svm(Y~.,data.frame(Xpp, Y), cost, ...)
    }
  })
  
  function(Xtrain, Ytrain, Xtest) {
    totalPred <- fitModels(subModels, Xtrain, Ytrain, n_Features)
    return(totalPred(Xtest))
  }
  
}


getModelRFTot <- function(n_Features, mtrys, ...) {
  library(randomForest)
  subModels <- lapply(mtrys, function(mtry) {
    function(Xpp, Y) {
      randomForest(Y~.,data.frame(Xpp, Y), mtry = mtry,  ...)
    }
  })
  
  function(Xtrain, Ytrain, Xtest) {
    totalPred <- fitModels(subModels, Xtrain, Ytrain, n_Features)
    return(totalPred(Xtest))
  }
}

