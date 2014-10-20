
if ( !exists("Xtot")) {
  trainingdata <- read.csv("Data/training.csv")
  testdata <- read.csv("Data/sorted_test.csv")
  
  soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")
  
  Xtot <- trainingdata[,!colnames(trainingdata) %in% soil_properties]
  Ytot <- trainingdata[,soil_properties]
  
}