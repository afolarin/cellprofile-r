###############################################################################
###############################################################################
############# Utility Functions For the Package CellProfile-R ##################
###############################################################################
# Author: Amos A Folarin (amosfolarin@gmail.com)
# Description: Utility functions for CellProfile-R R-package
# History:
#       - version 1.0
#       - 
###############################################################################
###############################################################################



###############################################################################
### chunk number: Performs basic checks on filename(s) arguements <INTERNAL>
#
# @arguements filenames is an character array of filenames
# @value boolean for check results
# @keyword internal
###############################################################################
checkValidFilenames <- function(filenames) 
{
    ## Returns TRUE if filenames is a character vector containing
    ## paths to files that exist (directories don't count).
    ## A suitable error message is printed via stop() if invalid
    ## file names are encountered.
    if (!is.character(filenames))
      stop(strwrap(paste("file names must be specified using a character",
             "vector, not a", sQuote(typeof(filenames)))), 
             call.=FALSE)

    if (length(filenames) == 0)
      stop("no file names provided")

    if (any(sapply(filenames, nchar) < 1))
      stop("empty file names are not allowed")

    finfo <- file.info(filenames)
    whBad <- sapply(finfo[["isdir"]], function(x) !identical(FALSE, x))
    if (any(whBad)) 
    {
    msg <- paste("the following are not valid files:\n",
             paste("  ", filenames[whBad], collapse="\n"))
    stop(msg, call.=FALSE)
    }
    TRUE
}





###############################################################################
### chunk number: Data median centering columns  <INTERNAL>
# d is the matrix, dataframe
# direction i.e. row or column
###############################################################################
center.on.median <- function(d, direction)
{
    if(direction == "column")
    {
        m <- apply(d, 2, median)
        d.m <- t(t(d) - m)
        return(d.m)    
    }
    else if(direction == "row")
    {
        m <- apply(d, 1, median)
        d.m <- d - m
        return(d.m)
    }
}