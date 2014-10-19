## --------------------------------------------------
## Coursera - R Programming - Assignment 1, pt. 2
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
## ------------------------------------------------

## --------------------------------------------------
## Creates the function "complete", which takes two arguments:
## "directory", and "id".
## --------------------------------------------------
complete <- function(directory, id = 1:332) {
    directory<-("./specdata/")
    ## set value for "directory"
    nobs.vector <- rep(0,length(id))
    ## creates a vector of zeroes of length = as.numeric"id"
    all.files <- paste(directory, as.character(list.files(directory)),sep="")
    ## creates a character vector of the file paths of each .csv file within the directory
    j <- 1
    ## for use in loop below
    for(i in id) {
        current.file<- read.csv(all.files[i], header=T, sep=",")
        ## reads in each ith .csv file from "specdata"
        nobs.vector[j] <- sum(complete.cases(current.file))
        ## appends to the jth element of nobs.vector the sum total of all non-NA entries in current.file
        j <- j + 1
        ## advances j one tick; debugged problem of [i] not necessarily being an integer
    }
    result<-data.frame(id=id, nobs = nobs.vector)
    ## result is a data frame with [,1]=id and [,2] = nobs
    return(result)
}

## --------------------------------------------------
## Tests 
# complete("specdata", 1)
# complete("specdata", c(2, 4, 8, 10, 12))
# complete("specdata", 30:25)
# complete("specdata", 3)
## --------------------------------------------------