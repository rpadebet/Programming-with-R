pollutantmean <- function (directory = "specdata",
                           pollutant = "sulfate",
                           id =c(1:length(list.files(directory)))) {
    
    # calculates the mean of a pollutant (sulfate or nitrate) 
    # across a specified list of monitors. 
    # The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
    # Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
    # particulate matter data from the directory specified in the 'directory' argument 
    # and returns the mean of the pollutant across all of the monitors,
    # ignoring any missing values coded as NA.
    
    # read the files and putting in a data frame
    dir <- paste0(getwd(),"/",directory,"/")
    files <- list.files(dir)
    
    data <- data.frame("Date" = c(),"sulfate"=c(),"nitrate" = c(),"ID" = c() )
    
    for (i in id) {
        data_temp <- read.csv(paste0(dir,files[i]),header = TRUE,stringsAsFactors = FALSE)
        data = rbind(data,data_temp)
    }
    
    names(data)<-c("Date","sulfate","nitrate","ID")    
    pollutantmean <- mean(data[,pollutant],na.rm = TRUE)  
   
    return(pollutantmean)    
}