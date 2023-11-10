##load_vector_using_rgdal
##load_raster_using_rgdal
##movecost script=group
##Area_of_Interest=vector polygon
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Moverank by Polygon=name
##Move=selection 16;8;4 ;
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##LCPN=number 3
##Use_Corridor=string TRUE
##Legend_Position=selection topright;bottomright;bottom;left;topleft;right;center ;
##Cognitive_Slope=string TRUE
##Add_Chart=string FALSE
##Bubble_cex=number 0.5
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9

##Output_LCP=output vector
##Output_Least_Cost_Corridor=output raster
##Output_DTM=output raster
##showplots
# Load required libraries
required_packages <- c("movecost", "sp", "progress", "raster")
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
#DTM <- raster(DTM)

# Get CRS from input vector (Origin)
origin_crs <- sp::proj4string(Origin)

# Set CRS for DTM to match Origin's CRS
raster::crs(Area_of_Interest) <- origin_crs
# Mappa i numeri in stringhe utilizzando la funzione di utilità
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)
# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)


# Convert string to logical
PlotBarrier <- as.logical(PlotBarrier)
# If PlotBarrier is FALSE, set Barrier and Field to NULL
if(!PlotBarrier) {
  Barrier <- NULL
 
} else {
  Move <- 8
}
IrregularDTM <- as.logical(IrregularDTM)
Cognitive_Slope <- as.logical(Cognitive_Slope)
Add_Chart <- as.logical(Add_Chart)
Use_Corridor <- as.logical(Use_Corridor)
# Esegui la funzione movecost
r <- moverank(
  dtm = NULL,
  origin = Origin,
  destin = Destination,
  studyplot = Area_of_Interest,
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  irregular.dtm = IrregularDTM,
  funct = Function,
  time = Time,
  lcp.n = LCPN,
  use.corr=Use_Corridor,  
  move = Move,
  cogn.slp = Cognitive_Slope,
  sl.crit = Critical_Slope,
  W = Walker_Body_Weight,
  L = Carried_Load_Weight,
  N = N,
  V = Speed,
  z = Zoom_Level,
  add.chart=Add_Chart,
  export = FALSE
)

b1.sp<-as(r$LCPs, "SpatialLinesDataFrame")
# Set the CRS for the Output_LCP to match Origin's CRS
sp::proj4string(b1.sp) <- origin_crs
Output_LCP=b1.sp
# Convert raster output to SpatialPixelsDataFrame and write to GeoTIFF
if(Use_Corridor==TRUE) {
    print(r$'least-cost corridor')
    raster_layer.sp <- as(r$'least-cost corridor',"SpatialPixelsDataFrame")
    sp::proj4string(raster_layer.sp) <- origin_crs
    Output_Least_Cost_Corridor=raster_layer.sp
} 
raster2.sp <- as(r$dtm, "SpatialPixelsDataFrame") 
#sp::proj4string(raster2) <- origin_crs
#ras2.sp <- crop(raster2.sp, DTM)
Output_DTM=raster2.sp