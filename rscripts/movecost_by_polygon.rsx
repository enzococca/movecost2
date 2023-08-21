##load_vector_using_rgdal
##load_raster_using_rgdal
##movecost script=group
##Area_of_interest=vector polygon
##Origin=vector point
##Destination=vector point
##Barrier=vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Movecost=name
##Move=number 16
##Field=number 0
##Breaks=number 0.5
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Outp=selection r;c ;
##Return_Base=string TRUE
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##RL=number 2
##CL=string TRUE
##DL=string TRUE
##CB=number 0.6
##CLL=number 0.6
##Output_Accum_Cost_Surface=output raster
##Output_Isoline=output vector
##Output_LCP=output vector
##Output_LCP_Back=output vector
##Output_W_Cost=output vector
##Output_DTM=output raster
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
# Get CRS from input vector (Origin)
origin_crs <- sp::proj4string(Origin)
# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)
time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)

Outp_map <- c("r", "c")
Outp <- get_string_value(Outp, Outp_map)
# Convert string to logical
PlotBarrier <- as.logical(PlotBarrier)
# If PlotBarrier is FALSE, set Barrier and Field to NULL
if(!PlotBarrier) {
  Barrier <- NULL
  Field <- NULL
} else {
  Move <- 8
}
IrregularDTM <- as.logical(IrregularDTM)
Return_Base <- as.logical(Return_Base)
Cognitive_Slope <- as.logical(Cognitive_Slope)
DL <- as.logical(DL)
CL <- as.logical(CL)
# Execute movecost function
r <- movecost(
  dtm = NULL,
  origin = Origin,
  destin = Destination,
  studyplot=Area_of_interest,  
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  irregular.dtm = IrregularDTM,
  field = Field,
  funct = Function,
  time = Time,
  outp = Outp,
  move = Move,
  breaks = Breaks,
  return.base = Return_Base,
  cogn.slp = Cognitive_Slope,
  sl.crit = Critical_Slope,
  W = Walker_Body_Weight,
  L = Carried_Load_Weight,
  N = N,
  V = Speed,
  z = Zoom_Level,
  rb.lty = RL,
  cont.lab = CL,
  destin.lab = DL,
  cex.breaks = CB,
  cex.lcp.lab = CLL,
  oneplot = TRUE,
  export = TRUE
)
warnings()

raster2.sp <- as(r$dtm, "SpatialPixelsDataFrame") 
ras2 <- crop(raster2.sp, Area_of_interest)
sp::proj4string(ras2.sp) <- origin_crs
Output_DTM=ras2.sp

raster.sp <- as(r$accumulated.cost.raster, "SpatialPixelsDataFrame") 
ras <- crop(raster.sp, Area_of_interest)
sp::proj4string(ras.sp) <- origin_crs
Output_Accum_Cost_Surface=ras.sp

a1.sp<-as(r$isolines, "SpatialLinesDataFrame")
# Set the CRS for the Output_Isoline to match Origin's CRS
sp::proj4string(a1.sp) <- origin_crs
Output_Isoline=a1.sp

b1.sp<-as(r$LCPs, "SpatialLinesDataFrame")
# Set the CRS for the Output_LCP to match Origin's CRS
sp::proj4string(b1.sp) <- origin_crs
Output_LCP=b1.sp

if(Return_Base==TRUE) {
    lback.sp <- as(r$LCPs.back, "SpatialLinesDataFrame")
    # Set the CRS for the Output_LCP_Back to match Origin's CRS
    sp::proj4string(lback.sp) <- origin_crs
    Output_LCP_Back=lback.sp
}

if(DL==TRUE) {
    # Add FID field
    
    dd.sp <- as(r$dest.loc.w.cost, "SpatialPointsDataFrame")
    # Set the CRS for the Output_W_Cost to match Origin's CRS
    sp::proj4string(dd.sp) <- origin_crs
    Output_W_Cost=dd.sp
}
