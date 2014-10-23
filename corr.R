## --------------------------------------------------
## Coursera - R Programming - Assignment 1, pt. 2
## 
## Kyle Scot Shank 
##
## 10/18/2014 
##
## Assignment:
## Write a function that takes a directory of data files and a 
## threshold for complete cases and calculates the correlation 
## between sulfate and nitrate for monitor locations where the 
## number of completely observed cases (on all variables) is 
## greater than the threshold. The function should return a vector 
## of correlations for the monitors that meet the threshold requirement. 
## If no monitors meet the threshold requirement, then the function 
## should return a numeric vector of length 0.
## --------------------------------------------------


assignmentURL <- paste('https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip')
## location of data on Coursera
tf <-  tempfile(fileext=".zip")
## Creates a placeholder file called "tf"
download.file(assignmentURL, tf)
## Downloads the file and places the zipped file into "tf"
unzip(tf, exdir= getwd(), overwrite=TRUE)
## Unzips the .zip file and places "specdata" into your current working directory.


corr<- function(directory, threshold) {
    complete.df <- complete("specdata", 1:332)
    ## utilizes "complete.R" to create a data frame of ids and observations
    valid.ids <- complete.df$id[complete.df$nobs > threshold]
    ## Creates an integer vector of all ids where nobs > threshold
    file.names <- list.files(directory)
    ## Read in all of the files currently in "specdata" directory
    file.numbers <- as.numeric(sub('\\.csv$','', file.names))
    ## Convert all of the elements of "file.names" vector to numeric
    ## by calling sub(). 
    selected.files <- file.names[match(valid.ids, file.numbers)]
    ## Subset the file(s) in "file.names" which exactly match the valid.ids
    above.threshold <- lapply(file.path(directory,selected.files), read.csv)
    ## Read the selected file(s) into the environment via read.csv (returns a list)
    calculate.cor = function(row) {
        cc = complete.cases(row)
        cor(row[cc,]$sulfate, row[cc,]$nitrate)
    }
    ## Function which calculcates correlation
    results <- sapply(above.threshold, calculate.cor)
    ## returns a vector of correlations
    return(results)
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