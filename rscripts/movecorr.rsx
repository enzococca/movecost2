##load_vector_using_rgdal
##load_raster_using_rgdal
##movecost script=group
##Movecorr=name
##DTM=raster
##Points=vector point
##Selection_ID_Point_A=number 1
##Selection_ID_Point_B=number 2
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Move=selection 16;8;4 ;
##Field=number 0
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9


##Output_LC_corridor=output raster
##Output_Accum_Cost_Surface_A=output raster
##Output_Accum_Cost_Surface_B=output raster
##Output_LCP_A_to_B=output vector
##Output_LCP_B_to_A=output vector



##showplots
# Load required libraries
required_packages <- c("movecost", "sp", "progress", "raster", "dplyr")
lapply(required_packages, require, character.only = TRUE)

# Load libraries
library(sp)
library(movecost)
library(progress)
library(raster)
library(dplyr)
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
# Convert string to logical
PlotBarrier <- as.logical(PlotBarrier)
# If PlotBarrier is FALSE, set Barrier and Field to NULL
if(PlotBarrier==TRUE) {
  Barrier <- NULL
  Field <- NULL
} else {
  Move <- 8
}
IrregularDTM <- as.logical(IrregularDTM)

Cognitive_Slope <- as.logical(Cognitive_Slope)
	
r<-movecorr(a=Points[Selection_ID_Point_A,],b=Points[Selection_ID_Point_B,],plot.barrier = PlotBarrier,
  barrier = Barrier,
  irregular.dtm = IrregularDTM,
  field = Field,funct=Function,time=Time,move=Move,dtm=DTM,cogn.slp=Cognitive_Slope,sl.crit=Critical_Slope,W=Walker_Body_Weight,L=Carried_Load_Weight,N=N,V=Speed,z=Zoom_Level)


LC.sp<- as(r$lc.corridor, "SpatialPixelsDataFrame") 
sp::proj4string(LC.sp) <- origin_crs
Output_LC_corridor=LC.sp

raster.sp <- as(r$accum_cost_surf_a, "SpatialPixelsDataFrame") 
sp::proj4string(raster.sp) <- origin_crs
Output_Accum_Cost_Surface_A=raster.sp

raster2.sp <- as(r$accum_cost_surf_b, "SpatialPixelsDataFrame") 
sp::proj4string(raster2.sp) <- origin_crs
Output_Accum_Cost_Surface_B=raster2.sp

a1.sp<-as(r$lcp_a_to_b, "SpatialLinesDataFrame")
sp::proj4string(a1.sp) <- origin_crs
Output_LCP_A_to_B=a1.sp

b1.sp<-as(r$lcp_b_to_a, "SpatialLinesDataFrame")
sp::proj4string(b1.sp) <- origin_crs
Output_LCP_B_to_A=b1.sp

