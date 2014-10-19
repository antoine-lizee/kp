d <- read.csv('training.csv')
wn.str <- names(d)[2:3579]

wn.col <- grep(colnames(d), pattern = "m.*")
wn.train <- d[,wn.col]
wn.pca <- prcomp(wn.train)

pca.rot <- wn.pca$rotation
matplot(pca.rot[,1:5], type = "l")

pca.vecs <- wn.pca$x

wn.features <- pca.vecs[,1:20]

# perform some principal component analysis to see what explains better

# controled sampling (or take measurement that have largest variance)
jump2take1 <- seq(1,3578,10)

form.temp <- paste(wn.str[jump2take1], collapse = " + ")
form <- paste("~ ",form.temp)
testdata <- d[,jump2take1 + 1]
pca <- prcomp(testdata,tol = .03,scale = T)
plot(pca)
str(pca)


plot(pca$rotation[,1], type = "l", col = "blue")
lines(pca$rotation[,2], type = "l", col = "red")

d$red.mes1 <- pca$x[,1]
d$red.mes2 <- pca$x[,2]
