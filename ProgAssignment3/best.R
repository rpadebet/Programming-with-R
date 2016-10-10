
best <- function(state = "NJ",outcome = "heart attack")
{
    ## Read outcome data
    hosp.data <- read.csv("hospital-data.csv", stringsAsFactors = FALSE)
    outcome.data <- read.csv("outcome-of-care-measures.csv",colClasses = "character", stringsAsFactors = FALSE)
    
    
     ## Check that state and outcome are valid
    
    valid.states <- unique(hosp.data[,"State"])
    valid.outcome <- c("heart attack","heart failure","pneumonia")
    
    try (if(!(state %in% valid.states)) stop("invalid state"))
    try (if(!(outcome %in% valid.outcome)) stop("invalid outcome"))
    
    ## Return hospital name in that state with lowest 30-day deathrate
    
    # clean the data to get only required columns and drop the NA's
    outcome.use <- outcome.data[,c(2,7,8,11,17,23)]
    names(outcome.use)<-c("Hospital","State","Zip","heart attack","heart failure","pneumonia")
  
    
    outcome.use.func <- outcome.use[outcome.use$State == state,c(TRUE,FALSE,TRUE,names(outcome.use[,4:6]) %in% outcome)]
    outcome.use.func[,3] <- as.numeric(outcome.use.func[,3],suppressWarnings = FALSE) 
    outcome.use.func <- outcome.use.func[complete.cases(outcome.use.func),]
    outcome.use.func <- outcome.use.func[order(outcome.use.func$Hospital),]
    outcome.use.func$Rank <- rank(outcome.use.func[,3],ties.method = "first")
    
    best.hospital <- outcome.use.func[outcome.use.func$Rank ==1,1]
    return(best.hospital)
}