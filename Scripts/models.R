
model <- function(Xtrain, Ytrain, Xtest) {
  wn.col <- grep(colnames(Xtrain), pattern = "m.*")
  wn.train <- d[,wn.col]
  wn.pca <- prcomp(wn.train)
  
  pca.rot <- wn.pca$rotation
  pca.vecs <- wn.pca$x
  
  preproc <- function(X) {
    return(pca.rot %*% X[,wn.col])
  }
  
  wn.features <- pca.vecs[,1:20]
  
  mod <- glm.fit(wn.features, Ytrain)
  
  return predict(mod, Xtest)
  
}

model_zero <- function(Xtrain, Ytrain, Xtest) {
  tmp <- matrix(0, nrow = dim(Xtest)[1], ncol = 5)
  Ypred <- as.data.frame(tmp)
  names(Ypred) <- c("Ca", "P", "pH", "SOC", "Sand")
  return(Ypred)
}
