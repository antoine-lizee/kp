
model <- function(Xtrain, Ytrain, Xtest) {
  
  center.b <- TRUE
  
  wn.col <- grep(colnames(Xtrain), pattern = "m.*")
  wn.train <- rbind(Xtrain[,wn.col], Xtest[,wn.col])
  wn.pca <- prcomp(wn.train, center = center.b)
  
  pca.rot <- wn.pca$rotation
  pca.vecs <- wn.pca$x
  
  #   matplot(pca.rot[,1:4], type = "l")
  
  preproc <- function(X) {
    if (center.b) {
      (data.matrix(X[,wn.col]) - (rep(1, nrow(X))  %o% wn.pca$center) )%*% pca.rot
    } else {
      data.matrix(X[,wn.col]) %*% pca.rot
    }
  }
  
  train <- function(Xt, Yt) {
    mod <- list()
    Xpp <- preproc(Xt)[,1:10]
    for (i in 1:ncol(Yt)) {
      mod[[i]] <- lm(Y~.,data.frame(Xpp, Y=Yt[,i]))
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
  
  MOD <- train(Xtrain, Ytrain)
  return(predict.mod(MOD, Xtest))
  
}

model_zero <- function(Xtrain, Ytrain, Xtest) {
  tmp <- matrix(0, nrow = dim(Xtest)[1], ncol = 5)
  Ypred <- as.data.frame(tmp)
  names(Ypred) <- c("Ca", "P", "pH", "SOC", "Sand")
  return(Ypred)
}
