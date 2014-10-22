


# Init --------------------------------------------------------------------
set.seed(27)
library(e1071)
source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

# Mutliple CV -------------------------------------------------------------
nFolds <- 5
PERF <- list()
PCAs <- c(10,20,40,80,160)
for (iPCA in 1:length(PCAs)) {
  PERF[[iPCA]] <- CVMultiple(list(getModelSVM(cost = 100, tolerance = 0.001),
                                  getModelSVM(cost = 10, tolerance = 0.001),
                                  getModelSVM(cost = 3, tolerance = 0.001),
                                  getModelSVM(cost = 1, tolerance = 0.001),
                                  getModelRF(ntree = 200, mtry = 12),
                                  getModelRF(ntree = 200, mtry = 14)),
                             Xtot,
                             Ytot,
                             nFold = nFolds,
                             PCA = PCAs[iPCA])
}

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

plotModels <- PERF[[1]]


# Compare -----------------------------------------------------------------

library(reshape2)
library(ggplot2)

data <- list()

for (iPCA in 1:length(PCAs)) {
  data[[iPCA]] <- cbind(melt(PERF[[iPCA]]$mean_cv_err, varnames = c("Feature", "Run"), value.name = "mean"), sd = melt(PERF[[iPCA]]$std_cv_err)[,"value"], PCA = PCAs[iPCA])
}

data_df <- do.call(rbind, data)

qplot(data = data_df, x = Run, y = mean, group = factor(Feature), color =  factor(Feature), geom = c("line")) +
  theme_bw() + facet_grid(~PCA)




