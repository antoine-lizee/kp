
model <- function(Xtrain, Ytrain, Xtest) {
  wn.str <- names(Xtrain)[2:3579]
  
  wn.col <- grep(colnames(Xtrain), pattern = "m.*")
  wn.train <- d[,wn.col]
  wn.pca <- prcomp(wn.train)
  
  pca.rot <- wn.pca$rotation
  pca.vecs <- wn.pca$x
  
  wn.features <- pca.vecs[,1:20]
  
  mod <- glm.fit(wn.features, )
  
  
}

