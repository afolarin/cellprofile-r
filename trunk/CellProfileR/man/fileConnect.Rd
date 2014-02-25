\name{fileConnect}
\alias{fileConnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a connection to a csv CellProfiler database
}
\description{
Provides an abstraction to a csv database created by the ExportToSpreadSheet CellProfiler Module. 
This is somewhat experiental as there is no facility for CellProfiler Analyst to utilize CSV
repositories of the data.

}
\usage{
fileConnect(directory, table.filenames = c(per_image = "", per_object = "", per_class = ""))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
        absoloute path to the directory with the csv table 
        files from cellprofiler, it's required that all files use the same directory
}
  \item{table.filenames}{
        the csv filename for both per_image 
        and per_object csv files e.g.
        c("per_image"="DefaultOUT_Image.csv", "per_object"="DefaultOUT_cell.csv").
        Additional tables to include in the connection may be spcified here with the
        same "key"="filepath" format.
        Note, a "per_class" table is however required if you want to use 
        createUnifiedTable see(createUnifiedTable).
}
}
\details{
This is a very simple abstraction of a database connection. Here implemented
simply as a path to the directory holding the relevant csv files. The paths to 
each of the files is pre-checked before the directory path is returned.
}
\value{
  a list of character vectors caching the directory path and table filenames.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
    \dontrun{
    con <- fileConnect(directory="<path>/cellprofilerOutputDir", table.filenames=c("per_image"="DefaultOUT_Image.csv", "per_object"="DefaultOUT_CellAndNuclei.csv"))
    }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
