##load_vector_using_rgdal
##load_raster_using_rgdal
##movecost script=group
##Movealloc=name
##DTM=raster
##Points=vector point
##Move=selection 16;8;4 ;
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Breaks=number 1
##Time=selection h;m ;
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1.2
##Zoom_Level=number 9
##Output_Isoline=output vector
##Output_Polygon=output vector
##Output_Alloc_Raster=output raster
##showplots
required_packages <- c("movecost", "sp", "progress", "raster", "rgdal")
lapply(required_packages, require, character.only = TRUE)

# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}
# Load libraries
library(sp)
library(movecost)
library(progress)
library(raster)


# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}

# Load input raster
DTM <- raster(DTM)

# Get CRS from input vector (Origin)
origin_crs <- sp::proj4string(Points)

# Set CRS for DTM to match Origin's CRS
raster::crs(DTM) <- origin_crs
# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
r<-movealloc(dtm=DTM, origin=Points, studyplot=NULL,funct=Function, cogn.slp=Cognitive_Slope,  sl.crit=Critical_Slope,W=Walker_Body_Weight, L=Carried_Load_Weight,N=N, V=Speed, z=Zoom_Level, cont.lab=TRUE, isolines=TRUE, breaks=Breaks, export=FALSE)


raster.sp <- as(r$cost.allocation.raster, "SpatialPixelsDataFrame") 
#ras.sp <- crop(raster.sp, Area_of_interest)
sp::proj4string(raster.sp) <- origin_crs
Output_Alloc_Raster=raster.sp

a1.sp<-as(r$isolines, "SpatialLinesDataFrame")
sp::proj4string(a1.sp) <- origin_crs
Output_Isoline=a1.sp

a2.sp<-as(r$alloc.boundaries, "SpatialPolygonsDataFrame")
sp::proj4string(a2.sp) <- origin_crs
Output_Polygon=a2.sp