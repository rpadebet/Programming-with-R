
rankall <- function(outcome = "heart attack",rank = 1)
{
    ## Read outcome data
    hosp.data <- read.csv("hospital-data.csv", stringsAsFactors = FALSE)
    outcome.data <- read.csv("outcome-of-care-measures.csv",colClasses = "character", stringsAsFactors = FALSE)
    
    
    valid.outcome <- c("heart attack","heart failure","pneumonia")
    
    try (if(!(outcome %in% valid.outcome)) stop("invalid outcome"))
    
    # clean the data to get only required columns and drop the NA's
    outcome.use <- outcome.data[,c(2,7,11,17,23)]
    names(outcome.use)<-c("Hospital","State","heart attack","heart failure","pneumonia")
    
    
    outcome.use.func <- outcome.use[ ,c(TRUE,TRUE,names(outcome.use[,3:5]) %in% outcome)]
    outcome.use.func[,3] <- as.numeric(outcome.use.func[,3])
    outcome.use.func$State <- as.factor(outcome.use.func$State)
    outcome.use.func <- outcome.use.func[complete.cases(outcome.use.func),]
    
    State.split <-split(outcome.use.func,outcome.use.func$State)
    
    result_df <- data.frame("Hospital" = character(),"State" = character())
    ranks <- vector(mode="integer",length(State.split))
    
    for(i in 1:length(State.split)){
        s_df<-State.split[[i]]
        s_df <- s_df[order(s_df$Hospital,decreasing = FALSE),]
        s_df$Rank <- rank(s_df[,3],ties.method = "first")
        
        if(rank == "best"){
            ranks[i] <- 1 
            } else if(rank == "worst"){
            ranks[i] = max(s_df$Rank)
            } else {
            ranks[i] = rank
            }
        
        s_df_res <- s_df[s_df$Rank==ranks[i],c(1,2)]
        
        if (nrow(s_df_res)==0)
        {
            s_df_res <- data.frame("Hospital" = "NA","State" = s_df$State[1])
            result_df<-rbind(result_df,s_df_res)
        }
        else
        {
            result_df<-rbind(result_df,s_df_res)  
        }
        
        
    }
    
    
   # try (if(rank >max(outcome.use.func$Rank)) stop("NA"))
    
    return(result_df)
}