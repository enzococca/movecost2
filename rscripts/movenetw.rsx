##load_vector_using_rgdal
##load_raster_using_rgdal
##movecost script=group
##DTM=raster
##Origin=vector point
##Network_type=selection allpairs;neigh ;
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Field=number 0
##Movenetw=name
##Move=selection 16;8;4 ;
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;wcs;ree;b;p;pcf;m;hrz;vl;ls;a ;
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_LCPs_netw=output vector
##Output_LCPs_netw_merged=output vector
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
DTM <- raster(DTM)

# Get CRS from input vector (Origin)
origin_crs <- sp::proj4string(Origin)

# Set CRS for DTM to match Origin's CRS
raster::crs(DTM) <- origin_crs

# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
Netype_map <- c("allpairs", "neigh")
Network_type <- get_string_value(Network_type, Netype_map)
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

Cognitive_Slope <- as.logical(Cognitive_Slope)

r<-movenetw(
  dtm=DTM, 
  origin=Origin, 
  netw.type=Network_type,
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  irregular.dtm = IrregularDTM,
  field = Field, 
  funct=Function,
  move=Move, 
  cogn.slp=Cognitive_Slope, 
  sl.crit=Critical_Slope, 
  W=Walker_Body_Weight, 
  L=Carried_Load_Weight,
  N=N, 
  V=Speed, 
  z=Zoom_Level, 
  lcp.dens=TRUE, 
  export=FALSE)

if (Network_type == 'allpairs') {
  if (!is.null(r$LCPs.netw.merged)) {
    # Create data.frame with row names matching the line IDs
    df <- data.frame(id = 1:length(r$LCPs.netw.merged))
    row.names(df) <- sapply(slot(r$LCPs.netw.merged, "lines"), function(x) slot(x, "ID"))

    # Convert SpatialLines to SpatialLinesDataFrame
    Output_LCPs_netw_merged <- SpatialLinesDataFrame(r$LCPs.netw.merged, data = df)
    sp::proj4string(Output_LCPs_netw_merged) <- origin_crs
  } else {
    # Handle the case where r$LCPs.netw.merged is NULL
    Output_LCPs_netw_merged <- NULL
    print("r$LCPs.netw.merged is NULL, so no SpatialLinesDataFrame was created.")
  }

    # Combine list of SpatialLines objects into one SpatialLines object
    a1 <- do.call(raster::bind, r$LCPs.netw)

    Output_LCPs_netw=a1# Create empty data.frame with row number equal to the number of SpatialLines
    df <- data.frame(id = seq_along(a1))

    # Convert SpatialLines to SpatialLinesDataFrame
    a1_df <- SpatialLinesDataFrame(a1, data = df)
    sp::proj4string(a1_df) <- origin_crs
    # Now a1_df is a SpatialLinesDataFrame
    Output_LCPs_netw <- a1_df
} else {
  if (!is.null(r$LCPs.netw.neigh.merged)) {
    # Create data.frame with row names matching the line IDs
    df <- data.frame(id = 1:length(r$LCPs.netw.neigh.merged))
    row.names(df) <- sapply(slot(r$LCPs.netw.neigh.merged, "lines"), function(x) slot(x, "ID"))

    # Convert SpatialLines to SpatialLinesDataFrame
    Output_LCPs_netw_merged <- SpatialLinesDataFrame(r$LCPs.netw.neigh.merged, data = df)
    sp::proj4string(Output_LCPs_netw_merged) <- origin_crs
  } else {
    # Handle the case where r$LCPs.netw.neigh.merged is NULL
    Output_LCPs_netw_merged <- NULL
    print("r$LCPs.netw.neigh.merged is NULL, so no SpatialLinesDataFrame was created.")
  }

  # Combine list of SpatialLines objects into one SpatialLines object
    a1 <- do.call(raster::bind, r$LCPs.netw.neigh)

    Output_LCPs_netw=a1# Create empty data.frame with row number equal to the number of SpatialLines
    df <- data.frame(id = seq_along(a1))

    # Convert SpatialLines to SpatialLinesDataFrame
    a1_df <- SpatialLinesDataFrame(a1, data = df)
    sp::proj4string(a1_df) <- origin_crs
    # Now a1_df is a SpatialLinesDataFrame
    Output_LCPs_netw <- a1_df
}


