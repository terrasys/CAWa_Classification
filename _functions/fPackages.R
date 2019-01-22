#######################################################################################################
#######################################################################################################
#######################################################################################################
#Function for activating and loading packages 
#######################################################################################################
#######################################################################################################
#######################################################################################################
#packages
#######################################################################################################
loadandinstall <- function(mypkg) {
  if (!is.element(mypkg,installed.packages()[,1])){
    install.packages(mypkg)};
  library(mypkg, character.only=TRUE)}
pk <- c("classInt",
        "caret",
        "dplyr",
        "devtools",
        "ggplot2",
        "gtools",
        "lattice",
        "maptools",
        "mclust",
        "doParallel",
        "parallel",
        "pheno",
        "plyr",
        "randomForest",
        "raster",
        "RColorBrewer",
        "reshape",
        "rgdal",
        "rgeos",
        "RSAGA",
        "RStoolbox",
        "sf",
        "shapefiles",
        "snow",
        "sp",
        "tictoc",
        "TSclust",
        "zoo")
for(i in pk){loadandinstall(i)}

#Feature scale function
#-----------------------------------------------------------------------------------------------------
featureScale <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
#-----------------------------------------------------------------------------------------------------
#rsaga environment
#-----------------------------------------------------------------------------------------------------
myenv <- rsaga.env(workspace=W.DIR, 
                   path="c:/_saga_222_x64",
                   modules = "c:/_saga_222_x64/modules",
                   parallel = TRUE)
#rsaga.get.libraries(path= myenv$modules)
#rsaga.get.lib.modules("grid_calculus", env =  myenv, interactive = FALSE)
#rsaga.get.usage("shapes_grid",2,env =  myenv)
