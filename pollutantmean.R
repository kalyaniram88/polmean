pollutantmean <- function (directory = "specdata",pollutant = "sulfate",id = 1:332) {
        filemeans <- c()
        root <- "e:/R/"
        directory <- paste( root, directory, "/", sep="" )
        setwd( directory )
        filenames <- as.character( list.files() )
        filepaths <- paste( directory, filenames, sep="" )
        for( i in id ) {
        	loadfile <- read.csv( filepaths[i] )
        	result <- loadfile[,pollutant]
        	valid <- result[ !is.na( result ) ]
        	filemeans <- c(filemeans, valid )
        }
        return( mean( filemeans ) )
}

