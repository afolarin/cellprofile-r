\name{CellTracking}
\docType{data}
\alias{CellTracking}
\alias{uni.tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CellProfiler data from a time series imageing study of a set of normal
fibroblast cells (see vignette data("workflow2")).
}
\description{
A time series of phase contrast images taken every 10 mins. The objective here
is to be able to track a subset of these cells, in this case the cells tracked
are in metaphase or any other phase. This is done by combining the information
from CellProfileR and CellProfileR Analyst
}
\usage{
data("CellTracking")
}
\format{
CellTracking is a dataset that holds a the data.frame uni.tab represenative of
the unified_table generated by joining the per_image, per_object and per_class
tables generated CellProfiler and CellProfiler Analyst.

The data.frame uni.tab has 7681 rows 28 columns.
\enumerate{
\item "ImageNumber"                                     
\item "ObjectNumber"                                    
\item "CellAndNuclei_Number_Object_Number"              
\item "CellAndNuclei_AreaShape_Area"                    
\item "CellAndNuclei_AreaShape_Eccentricity"            
\item "CellAndNuclei_AreaShape_EulerNumber"             
\item "CellAndNuclei_AreaShape_Extent"                  
\item "CellAndNuclei_AreaShape_FormFactor"              
\item "CellAndNuclei_AreaShape_MajorAxisLength"         
\item "CellAndNuclei_AreaShape_MinorAxisLength"         
\item "CellAndNuclei_AreaShape_Orientation"             
\item "CellAndNuclei_AreaShape_Perimeter"               
\item "CellAndNuclei_AreaShape_Solidity"                
\item "CellAndNuclei_Location_Center_X"                 
\item "CellAndNuclei_Location_Center_Y"                 
\item "CellAndNuclei_TrackObjects_DistanceTraveled_50"  
\item "CellAndNuclei_TrackObjects_IntegratedDistance_50"
\item "CellAndNuclei_TrackObjects_Label_50"             
\item "CellAndNuclei_TrackObjects_Lifetime_50"          
\item "CellAndNuclei_TrackObjects_Linearity_50"         
\item "CellAndNuclei_TrackObjects_Parent_50"            
\item "CellAndNuclei_TrackObjects_TrajectoryX_50"       
\item "CellAndNuclei_TrackObjects_TrajectoryY_50"       
\item "class"                                           
\item "class_number"                                    
\item "Image_Metadata_Hour"                             
\item "Image_Metadata_Plate"                            
\item "Image_Metadata_Well"                             
       

}

}

\source{
Unpublished data.
}

\references{
The CellTracking dataset was donated by Davide Danovi.
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 This dataset is used in the vignette for Workflow2.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
