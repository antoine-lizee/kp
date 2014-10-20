


# Init --------------------------------------------------------------------

source("Scripts//init.R")
source("Scripts//models.R")
source("Scripts//utils.R")

# linear Models -----------------------------------------------------------

writeSubmission(testdata$PIDN, model(Xtot, Ytot, testdata), "LM")

