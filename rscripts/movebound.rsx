##load_vector_using_rgdal
##load_raster_using_rgdal
##movecost script=group
##DTM=raster
##Points=vector point
##Movebound=name
##Barrier=optional vector
##PlotBarrier=string FALSE
##Area=string TRUE
##Move=selection 16;8;4 ;
##Cost_Value=number 1
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_Isoline=output vector

##showplots
# Load required libraries
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
library(rgdal)

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
# Converti la stringa di valori in un vettore numerico
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
  Field <- NULL
} else {
  Move <- 8
}

Cognitive_Slope <- as.logical(Cognitive_Slope)
Area <- as.logical(Area)
r<-movebound(dtm=DTM, 
  origin=Points, 
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  field = Field,
  funct=Function, 
  time=Time, move=Move, 
  cont.value=Cost_Value, 
  cogn.slp=Cognitive_Slope,  
  sl.crit=Critical_Slope,
  W=Walker_Body_Weight, 
  L=Carried_Load_Weight,
  N=N, V=Speed, 
  z=Zoom_Level, 
  cont.lab=TRUE, 
  add.geom=Area,
  export=FALSE)


a1.sp<-as(r$isolines, "SpatialLinesDataFrame")
# Set the CRS for the Output_Isoline ##Output_Isoline=output vectorto match Origin's CRS
sp::proj4string(a1.sp) <- origin_crs
Output_Isoline=a1.sp

if(Area==TRUE){

a2.sp<-as(r$origin_w_isolines_geom,'SpatialPointsDataFrame')
##Output_Area=output vector
sp::proj4string(a2.sp) <- origin_crs
Output_Area=a2.sp
}