
##movecost script=group
##Movebound=name
##CRS=crs
##DTM=raster
##Points=vector point
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
##Output_Area=output vector
##showplots
# Load required libraries
required_packages <- c("movecost", "sp", "sf","progress", "raster")
lapply(required_packages, require, character.only = TRUE)


# Load libraries
library(sp)
library(sf)
library(movecost)
library(progress)
library(raster)


# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}
# Get CRS from input vector (Origin)
# Assuming 'Origin' is an sf object and you need to get its CRS
origin_crs <- CRS
p <- as_Spatial(Points)
studyplot_sp <- raster(DTM)
crs(studyplot_sp) <- origin_crs
summary(studyplot_sp)  # Questo ti darà una panoramica dei dati, inclusi i valori min e max
any(is.na(studyplot_sp))  # Controlla se ci sono valori NA
any(studyplot_sp == Inf)  # Controlla se ci sono valori infiniti
studyplot_sp[is.na(studyplot_sp)] <- 0  # Sostituisce i valori NA con 0
studyplot_sp[studyplot_sp == Inf] <- max(studyplot_sp, na.rm = TRUE)  # Sostituisce i valori infiniti con il massimo valore non infinito
print(studyplot_sp)

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
r<-movebound(dtm=studyplot_sp,
  origin=p,
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


a1=r$isolines
sf_object = st_as_sf(a1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object))) {
  sf_object <- st_set_crs(sf_object, CRS) # esempio con WGS84
}

# Ora esporta il file
Output_Isoline=sf_object

if(Area==TRUE){

a2<-r$origin_w_isolines_geom
sf_object2 = st_as_sf(a2)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object2))) {
  sf_object2 <- st_set_crs(sf_object2, CRS) # esempio con WGS84
}

# Ora esporta il file
Output_Area=sf_object2
}