complete <- function (directory = "specdata",
                           id =c(1:length(list.files(directory)))) {
    
    # function that reads a directory full of files and 
    # reports the number of completely observed cases in each data file
    # The function returns a data frame where the first column is the name of the file
    # and the second column is the number of complete cases
    
    # read the files and putting in a data frame
    dir <- paste0(getwd(),"/",directory,"/")
    files <- list.files(dir)
    
    data <- data.frame("Name" = c(),"nobs" = c())
    
    for (i in id) {
        data_temp <- read.csv(paste0(dir,files[i]),header = TRUE,stringsAsFactors = FALSE)
        data_clean <- complete.cases(data_temp)
        data = rbind(data,c(i,sum(data_clean)))
    }
    names(data)<-c("id","nobs")
    
    return(data)    
}