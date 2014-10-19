Y <- read.csv("Output/sample_submission.csv")
Y.hat <- read.csv("Data/submission.csv")

mcrmse <- function(Y,Y.hat){
  # Y and Y.hat have 6 columns (PIDN,Ca,P,pH,SOC,Sand)
  error <- numeric(0)
  for (i in 2:6){
    error <- c(error, sqrt(mean((Y[i] - Y.hat[i])^2)))
  }
  error <- c(error, mean(error))
  names(error) <- c("mseCa","mseP", "msepH", "mseSOC", "mseSand", "mcrmse")
  return(error)
}
mcrmse(Y,Y.hat)

