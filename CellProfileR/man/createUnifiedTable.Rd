\name{createUnifiedTable}
\alias{createUnifiedTable}
\title{
Create a unified table in the CellProfiler Database
}
\description{
Carry out a thee way TABLE JOIN on per_object, per_image, class_table (see CP properties file),
this function should work equally for MySQL, SQLite and CSV repositories. 
See details.
}
\usage{
createUnifiedTable(database.connection, class.table.name, per.object.cols = c("*"), per.image.cols = c("*"))
}
\arguments{
  \item{database.connection}{
        a connection object returned from 
        mySqLiteConnect, sqLiteConnect, fileConnect.
}
  \item{class.table.name}{
        the name given to the class table in the 
        CellProfiler Properties File, or if using csv then use the filename.
}
  \item{per.object.cols}{
        character vector of per_object table columns
        to include in unified table, defaults to all columns if not specified.
}
  \item{per.image.cols}{
        character vector of per_image table columns to 
        include in unified table, If using CellProfiler METADATA 
        capture, you will most likely want to include these columns! Defaults to
        all columns if not specified.
}
}
\details{
Performs a database join on three tables: per_object, per_image, class_table, 
the columns are merged on the ImageNumber primarily, and objects secondly on 
the ObjectNumber. The resulting table is written to the database with the 
hardcoded name "unified_table". The unified_table can be read in with \code{\link{loadTable}}. 
Note: it is required to have created a class_table
which requires that you have run CellProfiler Analyst Classifier 
(see the properties file "class_table"), this option may not yet be available
if you are using CSV database options.

The unified_table will typically have all columns from per_object, a specified
number of columns from per_image and always class_table 
("class" and "class_number") columns.


}
\value{
    if successful, returns an vector of column names from the create unified table
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
    See Also as \code{\link{loadTable}}
}
\examples{

\dontrun{
    con <- mySqlConnect("localhost", "db_name", "db_username", "db_password")
    createUnifiedTable(con, "per_class", per.image.cols=c("Image_Metadata_Hour", "Image_Metadata_Plate", "Image_Metadata_Well", "Image_PathName_Phase","Image_FileName_Phase"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

