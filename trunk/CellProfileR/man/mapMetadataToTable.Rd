\name{mapMetadataToTable}
\alias{mapMetadataToTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Maps onto a database table in R some user supplied metadata
}
\description{
It may be convenient map metadata onto a column of a database table loaded  
  e.g. per_image or unified.table etc.) into R, this makes it easy to access  
  this information if the table is remodelled or split. This is particularly 
  useful where a specific experimental structure is used e.g. plates are used, 
  and replicate experiments are contained; or to define annotation for the experiment. 
}
\usage{
mapMetadataToTable(map.filename, table.data, table.indices, metadata.col = "mapped.metadata")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map.filename}{
  a mapping filename. This file contains the indices and metadata
  to map to the loaded database table.
}
  \item{table.data}{
      the \code{data.frame} table which you want add an experiment mapping to e.g.
      the per_image table. The \code{table.data} must have the corresponding
      indices (e.g. Plate, Well) columns specified in the file \code{map.filename}.
}
  \item{table.indices}{
      a string vector of column names of the indices in \code{table.data} 
      to the metadata e.g. Image_Metadata_Plate, Image_Metadata_Well.
      
}
  \item{metadata.col}{
      a user specified name for the metadata column to be added to returned table.
    } 
}

\details{
For use on a loaded database table, this function facilitates mapping on additional metadata columns through a supplied set.
The format for the mapping file should be any number of index columns 
(i.e. which combined reference a particular metadata element) with the 
last column providing the metadtaa to map. The indices used must also be 
identical to those present in the database.

}
\value{
    returned table.data with a new metadata \code{metadata.col} column added.
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
    mdata <- mapMetadataToTable("../data/example_plate_map_file.txt", table.data=per.img, table.indices=c("Image_Metadata_Plate","Image_Metadata_Well"), metadata.col="mapped.metadata")
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
