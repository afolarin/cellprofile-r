\name{CellProfileR-package}
\alias{CellProfileR-package}
\alias{CellProfileR}
\docType{package}
\title{
An interface in R for CellProfiler and CellProfiler Analyst Databases
}
\description{
CellProfiler (CP) and CellProfiler Analyst(CPA) are tools for analysing image 
sets of cells. The software is designed to enable biologists to easily quantitate 
phenotypes from thousands of images automatically. CellProfileR, a library for 
the statistical programming environment R, enables the users of CP/CPA to easily 
interface their databases with R. This is particularly useful where more 
sophisticated downstream analyses are required. 
Convenience functions and a number of example workflows are defined in the package.
}
\details{
\tabular{ll}{
Package: \tab CellProfileR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-03-11\cr
License: \tab GPL-2 \cr
LazyLoad: \tab yes\cr
}
CellProfileR can be used to load and manipulate databases created with CellProfiler and CellProfiler Analyst.

With the exception of loadTable, refer to the DBI functions for other database manipulation use
the DBI functions for writing and removing tables:

  # save a dataframe from R as a table 
  dbWriteTable(con, name, value, ...)
  # list the tables in the database
  dbListTables(con)
  # verify a table exists in the database
  dbExistsTable(con, name, ...)
  # remove a table from the database
  dbRemoveTable(con, name, ...)
  
}
\author{
Amos Folarin

Maintainer: Amos Folarin <\email{amosfolarin+cellprofile-r@gmail.com}>
}
\references{
    \itemize{
        
    \item GoogleCode SVN Repository for this package (CellProfileR), see \url{http://code.google.com/p/cellprofile-r/}
    
    \item See for further details of the CellProfiler and CellProfiler Analyst applications:
    CellProfiler Website \url{http://cellprofiler.org/index.shtml}
    
    \item Carpenter AE, Jones TR, Lamprecht MR, Clarke C, Kang IH, Friman O, Guertin DA, Chang JH, Lindquist RA, Moffat J, Golland P, Sabatini DM (2006) CellProfiler: image analysis software for identifying and quantifying cell phenotypes. Genome Biology 7:R100. PMID: 17076895
    
    \item Kamentsky L, Jones TR, Fraser A, Bray M, Logan D, Madden K, Ljosa V, Rueden C, Harris GB, Eliceiri K, Carpenter AE (2011) Improved structure, function, and compatibility for CellProfiler: modular high-throughput image analysis software. Bioinformatics 2011/doi. PMID: 21349861

    }
}

\keyword{CellProfileR}
\seealso{}
\examples{}
