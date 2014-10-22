


# Init --------------------------------------------------------------------
set.seed(27)
library(e1071)
library(gbm)
source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

# Mutliple CV -------------------------------------------------------------
nFolds <- 5
PCAs <- c(10,20,40,80,160)
PERF2 <- CVMultiple(list(getModelRFTot(n_Features = list(10,10,10,10,10), mtrys = rep(8,5)),
                         getModelRFTot(n_Features = list(200,200,200,200,200), mtrys = rep(16,5)),
                         getModelSVMTot(n_Features = list(20,20,20,20,20), 
                                        costs = c(10, 10, 10, 10, 3)),
                         getModelSVMTot(n_Features = list(80,80,80,80,80), 
                                        costs = c(10, 10, 10, 10, 10))),
                    Xtot,
                    Ytot,
                    nFold = nFolds)




# Plot one PERF -----------------------------------------------------------


plotModels <- function(PERF2, sd = FALSE) {
  data_mean_sd <- cbind(melt(PERF2$mean_cv_err, varnames = c("Feature", "Run"), value.name = "mean"), sd = melt(PERF2$std_cv_err)[,"value"])
  data_mean_sd$se <- data_mean_sd$sd / nFolds
  if (sd)  {
    qplot(data = data_mean_sd, x = Run, y = mean, ymin = mean-se, ymax = mean+se, group = factor(Feature), color =  factor(Feature), geom = c("line", "errorbar")) +
      theme_bw()
  } else {
    qplot(data = data_mean_sd, x = Run, y = mean, ymin = mean-se, ymax = mean+se, group = factor(Feature), color =  factor(Feature), geom = c("line")) +
      theme_bw()
  }
}

plotModels(PERF2)

