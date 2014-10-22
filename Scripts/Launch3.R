


# Init --------------------------------------------------------------------
set.seed(27)
library(e1071)
source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

# Mutliple CV -------------------------------------------------------------
nFolds <- 5
PCAs <- c(10,20,40,80,160)
PERF2 <- CVMultiple(list(getModelRF(n_Features = 15, ntree = 200, mtry = 12),
                        getModelRF(n_Features = 20, ntree = 200, mtry = 12),
                        getModelRF(n_Features = 25, ntree = 200, mtry = 12),
                        getModelRF(n_Features = 15, ntree = 200, mtry = 14),
                        getModelRF(n_Features = 20, ntree = 200, mtry = 14),
                        getModelRF(n_Features = 25, ntree = 200, mtry = 14)),
                   Xtot,
                   Ytot,
                   nFold = nFolds)

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

plotModels(PERF2)


# Compare -----------------------------------------------------------------

library(reshape2)
library(ggplot2)

data <- list()

for (iPCA in 1:length(PCAs)) {
  data[[iPCA]] <- cbind(melt(PERF[[iPCA]]$mean_cv_err, varnames = c("Feature", "Run"), value.name = "mean"), sd = melt(PERF[[iPCA]]$std_cv_err)[,"value"], PCA = PCAs[iPCA])
}

do.call(rbind, data)

qplot(data = data, x = Run, y = mean, ymin = mean-se, ymax = mean+se, group = factor(Feature), color =  factor(Feature), geom = c("line")) +
  theme_bw() + facet_wrap(~PCA)




