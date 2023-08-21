##load_vector_using_rgdal
##load_raster_using_rgdal
##movecost script=group
##DTM=raster
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Movecost=name
##Move=selection 16;8;4 ;
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


##showplots
# Load required libraries
required_packages <- c("movecost", "sp", "progress", "raster", "rgdal","dplyr")
lapply(required_packages, require, character.only = TRUE)

# Load libraries
library(sp)
library(movecost)
library(progress)
library(raster)
library(rgdal)
library(dplyr)
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

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
Outp_map <- c("r", "c")
Outp <- get_string_value(Outp, Outp_map)

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
Return_Base <- as.logical(Return_Base)
Cognitive_Slope <- as.logical(Cognitive_Slope)
DL <- as.logical(DL)
CL <- as.logical(CL)
if (!is.null(Barrier)) {
  # Il parametro Barrier è stato specificato dall'utente, quindi utilizza il suo valore
  # Puoi procedere con l'uso di Barrier nel tuo codice
} else {
  # Il parametro Barrier non è stato specificato dall'utente, quindi non viene utilizzato
  # In questo caso, il calcolo sarà eseguito senza considerare la barriera.
}

# Execute movecost function
r <- movecost(
  dtm = DTM,
  origin = Origin,
  destin = Destination,  
  plot.barrier = PlotBarrier,
  barrier = Barrier,
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
  oneplot = FALSE,
  export = FALSE
)
warnings()

dtm_r <- movecost(
  dtm = DTM,
  origin = Origin,
  destin = Destination,
  
)
# Convert raster output to SpatialPixelsDataFrame and write to GeoTIFF
raster.sp <- as(r$accumulated.cost.raster, "SpatialPixelsDataFrame") 
# Set the CRS for the Output_Accum_Cost_Surface to match Origin's CRS
sp::proj4string(raster.sp) <- origin_crs
Output_Accum_Cost_Surface=raster.sp

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
    
    dd.sp <- as(r$dest.loc.w.cost, "SpatialPointsDataFrame")
    # Set the CRS for the Output_W_Cost to match Origin's CRS
    sp::proj4string(dd.sp) <- origin_crs
    #Output_W_Cost = dd.sp
    converti_tempo <- function(tempo_giorni) {
          giorni <- floor(tempo_giorni)
          ore_decimali <- (tempo_giorni - giorni) * 24

          ore <- floor(ore_decimali)
          minuti <- floor((ore_decimali - ore) * 60)
          secondi <- round((ore_decimali * 60 - minuti) * 60)

          # Gestione dei plurali per giorni e ore
          giorni_label <- ifelse(giorni == 1, "giorno", "giorni")
          ore_label <- ifelse(ore == 1, "ora", "ore")

          tempo_convertito <- paste(
            if (giorni > 0) paste(giorni, giorni_label) else NULL,
            if (ore > 0) paste(ore, ore_label) else NULL,
            if (minuti > 0) paste(minuti, "minuti") else NULL,
            if (secondi > 0) paste(secondi, "secondi") else NULL,
            sep = " "
          )

          return(tempo_convertito)
    }


    
    # Converti la colonna cost_hms in numeri
    dd.sp$cost_hms <- as.numeric(dd.sp$cost_hms)

    # Verifica se ci sono valori non numerici o mancanti nella colonna
    valori_non_numerici <- !is.na(dd.sp$cost_hms) & !is.numeric(dd.sp$cost_hms)

    # Visualizza i valori non numerici o mancanti
    print(dd.sp[valori_non_numerici, ])

    dd.sp$tempo_convertito <- sapply(dd.sp$cost_hms, converti_tempo)

    print(dd.sp)
    
    # Assegna il SpatialPointsDataFrame con la colonna aggiunta al tuo Output_W_Cost
    Output_W_Cost<-dd.sp
    Output_W_Cost=Output_W_Cost
}