###############################################################################
###############################################################################
############### Plate Mapping and Table Reorganization Functions ##############
###############################################################################
# Author: Amos A Folarin (amosfolarin@gmail.com)
# Description: Mapping experimental metadata onto the tables. 
# This is particularly geared towards experiments that utilizem multi-well 
#   plates, time series etc. Convenience functions are also provided 
#   for re-organizing tabular data. 
# History:
#       - version 1.0
#       - 
###############################################################################
###############################################################################


## TODO
## create an example dataset using 10 timepoints from the CB660 data of the EMD screen. only select 2 plates... you can use the mapping file for this too...



###############################################################################
### chunk number: readPlateMapping <INTERNAL>
# Read in a plate well mapping which can be used to label the plate.
# The file is a tab delimited file of (plate, well) -> experiment mappings
# Format for the input should be one or more index columns with the metadata 
#   in the last column
# <1.Plate> <2.Wells> <3.Metadata>
# 
# e.g. 
#    "Plate1"    "A1"    "temozolomide100nm"
#    "Plate1"    "A2"    "pdgfri"
#    "Plate2"    "B1"    "cdki"
#    "Plate2"    "B2"    "taxol"
# an e.g. file is provided in the data directory (example_plate_map_file.txt)

# @arguements map.filename = mapping file that links Wells to 
#   Experimental Conditions
# @value dataframe containing the well->experimental conditions mapping
# e.g. 
# map.tab <- readPlateMapping(map.filename="experiment_well-map.txt")
###############################################################################
readPlateMapping <- function(map.filename)
{    
    map<- NULL
    
    if(!checkValidFilenames(map.filename))
    {
        stop("invalid filename for mapping file")
    }else if(length(map.filename) > 1)
    {
        stop("more than one filename supplied")
    }
    else
    {
        map <- read.table(map.filename, stringsAsFactors=FALSE, header=FALSE)
    }
    
    
    return(map)
}


###############################################################################
### chunk number: mapMetadataToTable
# It may be convenient map metadata onto a column of a database table loaded  
#   e.g. per_image or unified.table etc.) into R, this makes it easy to access  
#   this information if the table is remodelled or split. This is particularly 
#   useful where e.g. plates are used, and replicate experiments are contained 
#   on the same plate - this covariate can then be used to average repliates.
#
# Format for the mapping should be any number of index columns (i.e.referencing 
#   a particular experiment) with the last column providing the information to
#   map. The indices used must also be identical to those present in
#   the database. 
#   e.g. here 2 indices + some experimental metadata 
#   <1.Plate> <2.Wells> <3.Metadata>
# 
#    "Plate1"    "A1"    "temozolomide100nm"
#    "Plate1"    "A2"    "pdgfri"
#    "Plate2"    "B1"    "cdki"
#    "Plate2"    "B2"    "taxol"
# an e.g. file is provided in the data directory (example_plate_map_file.txt)
#
# @arguements map.filename = a mapping filename. This file contains the indices
#   necessary to map the metadata to the database table.
# @arguements table.data = the table which you want add an experiment mapping to 
#   e.g the per_image table. The table.dat must have the necessary metadata
#   (i.e. Plate and Well) columns.
# @arguements table.metadata.indices = a character vector of column names 
#   holding the index to the metadata e.g. Image_Metadata_Plate, 
# Image_Metadata_Well. These indices must match the order of the 
#   mapping file columns.
# @value return the table.data with a new metadata column bound.
# e.g.
# mdata <- mapMetadataToTable("../data/example_plate_map_file.txt", table.data=per.img, table.indices=c("Image_Metadata_Plate","Image_Metadata_Well"), metadata.col="mapped.metadata")
###############################################################################
mapMetadataToTable <- function(map.filename, table.data, table.indices, metadata.col="mapped.metadata")
{
    map <- readPlateMapping(map.filename)
    plate.map <- character(dim(table.data)[1]) # store experiments name for the table
        
    #join plate and well to make single id <Plate>|<Well> e.g. Plate1|A1
    map.plate.well.id <- apply(map[, -ncol(map)], 1, paste, collapse=":")
    
    #concatenate the plate and well fields on the table to give a new column
    table.plate.well.id <- apply(table.data[, table.indices], 1, paste, collapse=":")
    
    mapping <- match(table.plate.well.id, map.plate.well.id)
    
    ## assign the experiments to the corresponding well addressings on the plate.map
    plate.map[!is.na(mapping)] <- map[[3]][na.omit(mapping)] 
    
    return(cbind(table.data, metadata.col=plate.map))  ## or should we cbind this onto the table.data?  #TODO
    
}











