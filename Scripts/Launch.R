


# Init --------------------------------------------------------------------
set.seed(27)
library(e1071)
source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

# linear Models -----------------------------------------------------------

CV(model,Xtot,Ytot)
#writeSubmission(testdata$PIDN, model(Xtot, Ytot, testdata), "LM")

