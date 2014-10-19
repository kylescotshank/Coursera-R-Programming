## --------------------------------------------------
## Coursera - R Programming - Assignment 1, pt. 1
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

setwd("C://Users/Kyle Shank/Desktop/SCHOOL/COURSERA/RProgramming/Coding Assignment 1/coursera-r-programming/")

## --------------------------------------------------
## Creates the function "pollutantmean", which takes three arguments:
## "directory", "pollutant", and "id". 
## --------------------------------------------------
pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
    directory<-("./specdata/")
    ## set value for "directory"
    mean.vector <- c()
    ## initialize a vector to hold the pollutant data
    all.files <- paste(directory, as.character(list.files(directory)),sep="")
    ## creates a character vector of the file paths of each .csv file within the directory
    for(i in id) {
        current.file <- read.csv(all.files[i], header=T, sep=",")
        ## reads in each ith .csv file from "specdata"
        na.removed <- current.file[!is.na(current.file[, pollutant]), pollutant]
        ## removes NAs
        mean.vector <- c(mean.vector, na.removed)
        ## appends the mean of each na.removed vector to mean.vector
    }
    result <- mean(mean.vector)
    return(round(result,3))
    ## rounds results to match output from example output on Coursera       
}


## --------------------------------------------------
## Tests (From example output on Coursera, should all return TRUE)

## pollutantmean("specdata", "sulfate", 1:10) == 4.064
## pollutantmean("specdata", "nitrate", 70:72) == 1.706
## pollutantmean("specdata", "nitrate", 23) == 1.281
## --------------------------------------------------