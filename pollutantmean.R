## --------------------------------------------------
## Coursera - R Programming - Assignment 1, pt. 1
## 
## Kyle Scot Shank 
##
## 10/18/2014 
##
## Assignment:
## Write a function named 'pollutantmean' that calculates the mean 
## of a pollutant (sulfate or nitrate) across a specified list of monitors. 
## The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', 
## and 'id'. Given a vector of monitor ID numbers, 'pollutantmean' reads that monitors' 
## particulate matter data from the directory specified in the 'directory' argument 
## and returns the mean of the pollutant across all of the monitors, ignoring 
## any missing values coded as NA. 
## --------------------------------------------------

assignmentURL <- paste('https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip')
## location of data on Coursera
tf <-  tempfile(fileext=".zip")
## Creates a placeholder file called "tf"
download.file(assignmentURL, tf)
## Downloads the file and places the zipped file into "tf"
unzip(tf, exdir= getwd(), overwrite=TRUE)
## Unzips the .zip file and places "specdata" into your current working directory.

pollutantmean <- function(directory, pollutant, id = 1:332) {
    file.names <- list.files(directory)
    ## Read in all of the files currently in "specdata" directory
    file.numbers <- as.numeric(sub('\\.csv$','', file.names))
    ## Convert all of the elements of "file.names" vector to numeric
    ## by calling sub()
    selected.files <- file.names[match(id, file.numbers)]
    ## Subset the file(s) in "file.names" which exactly match the id provided 
    ## in the pollutantmean "id" argument.
    selected.list <- lapply(file.path(directory,selected.files), read.csv)
    ## Create the file path between the "specdata" directory and the selected file(s).
    ## Read the selected file(s) into the environment via read.csv (returns a list)
    filter.list <- sapply(selected.list, function(x) x[ ,pollutant])
    ## Filter the selected.list for the chosen "pollutant" column by calling
    ## the anonymous function function(x) x[ ,pollutant]; similiar to lambda(x) in Python
    results <- mean(unlist(filter.list), na.rm=TRUE)
    ## flatten filter.list into a numeric vector, find the mean of this vector, ignore NAs
    return(round(results,3))
    ## returns the result rounded to the third decimal place
}


## --------------------------------------------------
## Tests (From example output on Coursera, should all return TRUE)

## pollutantmean("specdata", "sulfate", 1:10) == 4.064
## pollutantmean("specdata", "nitrate", 70:72) == 1.706
## pollutantmean("specdata", "nitrate", 23) == 1.281
## --------------------------------------------------