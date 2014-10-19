library(stringr)
setwd("~/Desktop/Kaggle compet/Data")
d <- read.csv('training.csv')
head(d)
n <- nrow(d)


# look at the names
names(d)
# "PIDN"     "m7497.96" ....        "m599.76"   "BSAN"    
# "BSAS"     "BSAV"     "CTI"      "ELEV"     "EVI"      
# "LSTD"     "LSTN"     "REF1"     "REF2"     "REF3"    
# "REF7"     "RELI"     "TMAP"     "TMFI"     "Depth"   
# "Ca"       "P"        "pH"       "SOC"      "Sand"

# suggestion is to remove m2379.76 to m2352.76
i <- which(names(d) == "m2379.76")
j <- which(names(d) == "m2352.76")
d <- d[,-(i:j)]


# identify the last column of the MIR measurements
lastMIR <- which(names(d) == "m599.76")

# extract wave number from the names vector
wn.str <- names(d)[2:lastMIR]
wn.num <- sapply(wn.str,FUN = function(x) as.numeric(str_sub(x,2,-1)))

summary(wn.num)

# hava a look at the response curve

spl <- sample(n,20)
m <- apply(d[,2:lastMIR], 2, mean)
for (i in spl){
  if (i == spl[1]){
    plot(wn.num,d[i,2:lastMIR], type = "l")
  }
  lines(wn.num,d[i,2:lastMIR],col = i)
}
# plot the mean in bold
lines(wn.num,m,col = "blue", lwd =5)

# substract the mean to get more meaningful data
centered.wn <- d[,2:lastMIR]
for (i in 1:(lastMIR-1)){
  centered.wn[,i] <- centered.wn[,i] - m[i]
}
apply(centered.wn, 2, mean)

for (i in spl){
  if (i == spl[1]){
    plot(wn.num,centered.wn[i,], type = "l")
  }
  lines(wn.num,centered.wn[i,],col = i)
}

plot(apply(centered.wn, 2, sd), type = "l")




modelCa <- lm(Ca ~ red.mes1 + red.mes2 + 
                BSAN + BSAS + BSAV + CTI + 
                ELEV + EVI + LSTD + LSTN + 
                REF1 + REF2 + REF3 + REF7 + 
                RELI + TMAP + TMFI + Depth, data = d)
summary(modelCa)
modelCa <- step(modelCa)
summary(modelCa)

modelP <- lm(P ~ red.mes1 + red.mes2 + 
                BSAN + BSAS + BSAV + CTI + 
                ELEV + EVI + LSTD + LSTN + 
                REF1 + REF2 + REF3 + REF7 + 
                RELI + TMAP + TMFI + Depth, data = d)
summary(modelP)
modelP <- step(modelP)
summary(modelP)
plot(modelP)


modelPh <- lm(pH ~ red.mes1 + red.mes2 + 
               BSAN + BSAS + BSAV + CTI + 
               ELEV + EVI + LSTD + LSTN + 
               REF1 + REF2 + REF3 + REF7 + 
               RELI + TMAP + TMFI + Depth, data = d)
summary(modelPh)
modelPh <- step(modelPh)
summary(modelPh)
plot(modelPh)


modelSOC <- lm(SOC ~ red.mes1 + red.mes2 + 
                BSAN + BSAS + BSAV + CTI + 
                ELEV + EVI + LSTD + LSTN + 
                REF1 + REF2 + REF3 + REF7 + 
                RELI + TMAP + TMFI + Depth, data = d)
summary(modelSOC)
modelSOC <- step(modelSOC)
summary(modelSOC)
plot(modelSOC)

modelSand <- lm(Sand ~ red.mes1 + red.mes2 + 
                BSAN + BSAS + BSAV + CTI + 
                ELEV + EVI + LSTD + LSTN + 
                REF1 + REF2 + REF3 + REF7 + 
                RELI + TMAP + TMFI + Depth, data = d)
summary(modelSand)
modelSand <- step(modelSand)
summary(modelSand)
plot(modelSand)


test <- read.csv('sorted_test.csv')
head(test)
names(test)

rotated <- as.matrix(test[,jump2take1 + 1]) %*% pca$rotation[,1:2]
test$red.mes1 <- rotated[,1]
test$red.mes2 <- rotated[,2]

testCa <- predict(modelCa,newdata = test)
head(testCa)
testP <- predict(modelP,newdata = test)
head(testP)
testPh <- predict(modelPh,newdata = test)
head(testPh)
testSOC <- predict(modelSOC,newdata = test)
head(testSOC)
testSand <- predict(modelSand,newdata = test)
head(testSand)

submission <- data.frame(PIDN = test$PIDN,Ca = testCa, P = testP, pH = testPh, SOC = testSOC, Sand = testSand)
write.csv(submission, "submission.csv",row.names = F)
