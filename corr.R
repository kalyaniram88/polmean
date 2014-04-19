corr <- function (
        directory = "specdata",
        threshold = 1 ) {
        source("E:/R/specdata/complete.R")
  
        root <- "E:/R/"
        directory <- paste( root, directory, "/", sep="" )
        setwd( directory )
       
        filenames <- as.character( list.files() )
        filepaths <- paste( directory, filenames, sep="" )
        filecount <- length( list.files() )
        cases <- complete("specdata", id = 1:filecount)
        
        valid <- cases[,2]>=threshold
        
        validcases <- cbind(
                valid1 <- cases[,1][valid],
                valid2 <- cases[,2][valid]
        )
        
        colnames(validcases) <- c("id", "nobs")
        
        filenames <- filenames[ validcases[,1] ]
        
        filepaths <- paste( directory, filenames, sep="" )
        
        if( length( filenames ) == 0 ) {
                corrs = c()
                return( corrs )
        }
        
        corrs <- c()
        
        for( i in seq( filepaths ) ) {
                
                loadfile <- read.csv( filepaths[i] )
                
                sulfate <- loadfile$sulfate
                nitrate <- loadfile$nitrate
                
                corrs <- c( corrs, cor( sulfate[ complete.cases(sulfate,nitrate) ],
                                        nitrate[ complete.cases(sulfate,nitrate) ] ) )
        }
        
        return( corrs )
}
