
corr <- function(directory = "specdata",threshold = 0) 
    {
    
    
    # function that takes a directory of data files and a threshold for complete cases 
    # and calculates the correlation between sulfate and nitrate 
    # for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold.
    # The function returns a vector of correlations for the monitors that meet the threshold requirement. 
    # If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 
    dir <- paste0(getwd(),"/",directory,"/")
    files <- list.files(dir)
    
    corr <- as.numeric(c())
    for (i in 1:length(files)) {
        if(complete(directory,i)$nobs > threshold) 
        {
            data_temp <- read.csv(paste0(dir,files[i]),header = TRUE,stringsAsFactors = FALSE)
            data_clean <- data_temp[complete.cases(data_temp),]
            corr = c(corr,cor(data_clean[,"sulfate"],data_clean[,"nitrate"]))
        }
        else {
            corr = c(corr,0)
        }
        
    }
    return(corr[corr!=0])
    }

