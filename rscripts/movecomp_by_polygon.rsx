##load_vector_using_rgdal
##load_raster_using_rgdal
##Movecomp by Polygon=name
##movecost script=group
##Area_of_interest=vector polygon
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Movecomp by polygon=name
##Move=selection 16;8;4 ;
##Field=number 0
##Choice1=selection ; t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Choice2=selection ; t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Choice3=selection ; t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Return_Base=string TRUE
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_DTM=output raster
##Output_LCP=output vector
##Output_LCP_Back=output vector
##Output_DTM=output raster
# Load required libraries
required_packages <- c("movecost", "sp", "progress", "raster")
lapply(required_packages, require, character.only = TRUE)

# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}

# Load input raster
#DTM <- raster(DTM)
# Get CRS from input vector (Origin)
origin_crs <- sp::proj4string(Origin)
# Get CRS from input vector (Origin) and set CRS for DTM
#raster::crs(DTM) <- sp::proj4string(Origin)

# Map numbers to strings using utility function
function_map <- c("", "t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Choice1 <- get_string_value(Choice1, function_map)
Choice2 <- get_string_value(Choice2, function_map)
Choice3 <- get_string_value(Choice3, function_map)

# Define choices based on input
choices <- c(Choice1, Choice2)
if (Choice3 != '') choices <- c(choices, Choice3)

# Convert string to logical
logical_vars <- c("PlotBarrier", "IrregularDTM", "Return_Base", "Cognitive_Slope")
for (var in logical_vars) assign(var, as.logical(get(var)))

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
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
# Execute movecomp function with error handling
tryCatch({
  r <- movecomp(
    dtm = NULL, 
    origin = Origin, 
    destin = Destination,
    studyplot=Area_of_interest,
    barrier = Barrier,
    plot.barrier = PlotBarrier,
    irregular.dtm = IrregularDTM,
    field = Field, 
    choice = choices, 
    move = Move, 
    return.base = Return_Base, 
    cogn.slp = Cognitive_Slope, 
    sl.crit = Critical_Slope, 
    W = Walker_Body_Weight, 
    L = Carried_Load_Weight, 
    N = N, 
    V = Speed, 
    z = Zoom_Level, 
    oneplot = FALSE, 
    export = FALSE
  )
},
error = function(e) {
  message(str(e))
})

raster2.sp <- as(r$dtm, "SpatialPixelsDataFrame")
Output_DTM=raster2.sp

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

raster2.sp <- as(r$dtm, "SpatialPixelsDataFrame")
Output_DTM=raster2.sp