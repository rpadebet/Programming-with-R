
# Used to load the data, check the data and make some basic plots

hosp.data <- read.csv("hospital-data.csv", stringsAsFactors = FALSE)
outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character", stringsAsFactors = FALSE)

outcome[,11]<-as.numeric(outcome[,11])
hist(outcome[,11])

names(hosp.data)
names(outcome)

valid.states <- unique(hosp.data[,"State"])
