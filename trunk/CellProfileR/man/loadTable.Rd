\name{loadTable}
\alias{loadTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load a specified table from the database
}
\description{
Loads a specified set of columns from the given database table into
in R as a \code{data.frame}. This is abstracted for all three 
data repository types (MySQL, SQLite, CSV). 
}
\usage{
loadTable(database.connection, table.name, column.names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database.connection}{
            a connection object returned e.g. from 
            \code{mySQLConnect}, \code{mySQLiteConnect}, \code{fileConnect}.
}
  \item{table.name}{
           string name of the the table to load e.g. 
           "per_image", or for csv files the name of the file.
}
  \item{column.names}{
           a string vector of column names to load from the table.
}
}
\details{    

loadTable is similar to the DBI function dbReadTable(con, name), however here
you can explicitly select the columns you are interested in loading into R, this
may be useful where large tables are involved and you are only interested in
loading a subset into the R dataframe. 

For other functions relating to the manipulation of the underlying tables, use
the DBI functions for writing and removing tables.

  # save a dataframe from R as a table 
  dbWriteTable(con, name, value, ...)
  # list the tables in the database
  dbListTables(con)
  # verify a table exists in the database
  dbExistsTable(con, name, ...)
  # remove a table from the database
  dbRemoveTable(con, name, ...)
  
}
\value{
    Returns a table \code{table.name} from the database as a \code{data.frame}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Amos Folarin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See Also \code{mySqlConnect}, \code{sqLiteConnect}, \code{fileConnect} and
    the DBI functions: \code{dbReadTable}, \code{dbWriteTable},
    \code{dbRemoveTable}, \code{dbExistsTable}, \code{dbListTables} and
    \code{dbListFields}.
    
}
\examples{
    \dontrun{
    #mysqlExample
    database.connection <- 
        
    Load whole per_image table
    per.image <- loadTable(database.connection, table.name="per_image")
    Load column subset of per_object
    per.obj <- loadTable(database.connection, table.name="per_object",
    column.names=c("CellAndNuclei_AreaShape_Area",
    "CellAndNuclei_AreaShape_Eccentricity",
    "CellAndNuclei_AreaShape_EulerNumber"))
    }
    
    
    #A small example sqlite database is included in the inst/extdata folder (extdata 
    # dir moved to toplevel install dir after installation)
    # g144_db.sqlite, it is primarily to demonstrate the functions here not 
    # for any analytical use
    
    #-----mysqlite example------<runnable>

    #get the filepath to the data zip from the CellProfileR package
    db.file <- system.file("extdata","g144_db.sqlite", package="CellProfileR")
    
    #copy the sqlite database to tempdir
    file.copy(db.file, tempdir(), overwrite = TRUE, recursive = TRUE)
    
    #connect to this sqlite database &  load into R the different tables stored 
    con <- sqLiteConnect(database.file=file.path(tempdir(),"g144_db.sqlite"))
    per.img <- loadTable(con, "per_image")
    
    #load a subset of columns from per_image "ImageNumber" 
    # and "Mean_CellAndNuclei_AreaShape_Area"
    per.img.s <- loadTable(con, "per_image", c("ImageNumber",
    "Mean_CellAndNuclei_AreaShape_Area") )
    
    #load some other tables in the database
    per.obj <- loadTable(con, "per_image")
    
    per.class <- loadTable(con, "per_class")
    
    unified.table <- loadTable(con, "unified_table")
    
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
