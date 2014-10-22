


# Init --------------------------------------------------------------------
set.seed(27)
library(e1071)
source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

# Mutliple CV -------------------------------------------------------------
nFolds <- 8
PERF4 <- CVMultiple2(list(getModelSVM(n_Features = 20, cost = 10, tolerance = 0.001),
                          getModelSVM(n_Features = 15, cost = 10, tolerance = 0.001),
                          getModelSVM(n_Features = 25, cost = 10, tolerance = 0.001),
#                           getModelSVM(n_Features = 80, cost = 10, tolerance = 0.001),
#                           getModelSVM(n_Features = 256, cost = 10, tolerance = 0.001),
                          getModelRF(n_Features = 15, ntree = 200, mtry = 8),
                          getModelRF(n_Features = 40, ntree = 200, mtry = 14)),
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

print(plotModels(PERF4, TRUE))



