## --------------------------------------------------
## Coursera - R Programming - Assignment 1, pt. 3
## 
## Kyle Scot Shank 
##
## 10/18/2014 
## --------------------------------------------------

## --------------------------------------------------
## OPTIONAL: 
##
## Grab assignment .zip from Coursera, unzip and place into a 
## file called "specdata" in your current working directory


# assignmentURL <- paste('https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip')
# tf <-  tempfile(fileext=".zip")
#   # Creates a placeholder file called "tf"
# download.file(assignmentURL, tf)
#   # Downloads the file and places the zipped file into "tf"
# unzip(tf, exdir= getwd(), overwrite=TRUE)
#   # Unzips the .zip file and places "specdata" into your current working directory.
## --------------------------------------------------


corr <- function(directory, threshold = 0) {
    directory<-("./specdata/")
    ## set value for "directory"
    complete.df <- complete("specdata", 1:332)
    ## utilizes "complete.R" to create a data frame of ids and observations
    valid.ids <- complete_table$id[complete.df$nobs > threshold]
    ## Creates an integer vector of all ids where nobs > threshold
    loop.length <- length(valid.ids)
    corr.vector <- rep(0, loop.length)
    ## Creates an empty vector the length of the valid.ids vector
    all.files <- paste(directory, as.character(list.files(directory)),sep="")
    ## creates a character vector of the file paths of each .csv file within the directory
    j <- 1
    for(i in valid.ids) {
        current.file <- read.csv(all.files[i], header=T, sep=",")
        corr.vector[j] <- cor(current.file$sulfate, current.file$nitrate, use="complete.obs")
        j <- j + 1
    }
    result <- corr.vector
    return(result)
}
    
## --------------------------------------------------
## Tests
## cr <- corr("specdata", 150)
## head(cr)
## cr <- corr("specdata", 400)
## head(cr)
## cr <- corr("specdata", 5000)
## summary(cr)
## --------------------------------------------------