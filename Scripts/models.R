
model <- function(Xtrain, Ytrain, Xtest) {
  
  center.b <- TRUE
  
  wn.col <- grep(colnames(Xtrain), pattern = "m.*")
  wn.train <- Xtrain[,wn.col]
  wn.pca <- prcomp(wn.train, center = center.b)
  
  pca.rot <- wn.pca$rotation
  pca.vecs <- wn.pca$x
  
  matplot(pca.rot[,1:4], type = "l")
  
  preproc <- function(X) {
    if (center.b) {
      (data.matrix(X[,wn.col]) - (rep(1, nrow(X))  %o% wn.pca$center) )%*% pca.rot
    } else {
      data.matrix(X[,wn.col]) %*% pca.rot
    }
  }
  
  mod <- glm.fit(preproc(Xtrain)[,1:10], Ytrain)
  
  return(predict(mod, preproc(Xtest)[,1:10]))
  
}

model_zero <- function(Xtrain, Ytrain, Xtest) {
  tmp <- matrix(0, nrow = dim(Xtest)[1], ncol = 5)
  Ypred <- as.data.frame(tmp)
  names(Ypred) <- c("Ca", "P", "pH", "SOC", "Sand")
  return(Ypred)
}
