## --------------------------------------------------
## Coursera - R Programming - Assignment 1, pt. 2
## 
## Kyle Scot Shank 
##
## 10/18/2014 
##
## Assignment:
## Write a function that reads a directory full of files and
## reports the number of completely observed cases in each data file. 
## The function should return a data frame where the first column is 
## the name of the file and the second column is the number of complete
## cases. 
## --------------------------------------------------



assignmentURL <- paste('https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip')
## location of data on Coursera
tf <-  tempfile(fileext=".zip")
## Creates a placeholder file called "tf"
download.file(assignmentURL, tf)
## Downloads the file and places the zipped file into "tf"
unzip(tf, exdir= getwd(), overwrite=TRUE)
## Unzips the .zip file and places "specdata" into your current working directory.

complete<- function(directory, id = 1:332) {
    file.names <- list.files(directory)
    ## Read in all of the files currently in "specdata" directory
    file.numbers <- as.numeric(sub('\\.csv$','', file.names))
    ## Convert all of the elements of "file.names" vector to numeric
    ## by calling sub(). 
    selected.files <- file.names[match(id, file.numbers)]
    ## Subset the file(s) in "file.names" which exactly match the id provided 
    ## in the pollutantmean "id" argument.
    selected.list <- lapply(file.path(directory,selected.files), read.csv)
    ## Create the file path between the "specdata" directory and the selected file(s).
    ## Read the selected file(s) into the environment via read.csv (returns a list)
    nobs.complete.cases <- lapply(selected.list,complete.cases)
    ## Creates a logical list where TRUE indicates a value (from complete.cases() and FALSE
    ## indicates NA
    nobs.counts <- lapply(nobs.complete.cases,which)
    ## Count which row entries in nobs.complete.cases are TRUE
    nobs.vector <- sapply(nobs.counts,length)
    ## Creates a numeric vector the length of TRUE cases of nobs.complete.cases
    results <- data.frame(id = id, nobs = nobs.vector)
    ## Creates a data frame where the first column is the id and the second column is the number
    ## of complete cases.
    return(results)
    ## returns the result dataframe
}


## --------------------------------------------------
## Tests 
# complete("specdata", 1)
# complete("specdata", c(2, 4, 8, 10, 12))
# complete("specdata", 30:25)
# complete("specdata", 3)
## --------------------------------------------------