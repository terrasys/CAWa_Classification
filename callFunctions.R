#-----------------------------------------------------------------------------------------------------
print("Working directory, data and settings")
#-----------------------------------------------------------------------------------------------------
#working directory
W.DIR <- "d:/Dropbox/_git/CAWaClass/_data/"
#directory containing functions
FUNC.DIR <- "_functions/"
#directory containing input data
IN.DIR <- "_data/"
#directory containing results
OUT.DIR <- "_result/"
#name of reference unit shapefile
RU.SHP = "RU_fergana"
#name of pure sample file
PS  = "PureSample10-50.csv"
YEAR = 2015
#-----------------------------------------------------------------------------------------------------
print("Import functions")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackages.R"))
source(file.path(W.DIR,FUNC.DIR,"fPuSa.R"))
source(file.path(W.DIR,FUNC.DIR,"fZonalStatistics.R"))
source(file.path(W.DIR,FUNC.DIR,"fSample.R"))
source(file.path(W.DIR,FUNC.DIR,"fPuSaCompare.R"))
source(file.path(W.DIR,FUNC.DIR,"fModisClass.R"))
source(file.path(W.DIR,FUNC.DIR,"fClassAcc.R"))
#execute functions
#-------------------------------------------------------------------------------
print("1 | Dissimiliarity test of samples")
#-------------------------------------------------------------------------------
###Additional parameters
#CLASS.NAME - column name with class names
#TH - threshold which proportion of class-specific NDVI values should be considered (1=100%)

###Function
d <- fPuSa(W.DIR,
           IN.DIR ="_data/",
           OUT.DIR = "_result/",
           CLASS.NAME = "CLASS",
           PS,
           TH = 0.75,
           PLOT=FALSE)
print(d)
###Result
##dissimiliarity matrix -> "PureSample_DM.csv"

#-------------------------------------------------------------------------------
print("2 | Zonal statistics of reference units for MODIS imagery")
#-----------------------------------------------------------------------------------------------------
###Function
RU.MODIS <- fZonalStatistics(W.DIR,
                             IN.DIR  = "_data/",
                             OUT.DIR = "_result/",
                             RU.SHP = "RU_fergana",
                             YEAR = YEAR)

print(RU.MODIS)
###Result
#RU_fergana[YEAR].shp -> shape file with DOY-specific NDVI values 
#PureSample_NDVI-profiles.pdf -> class-specific NDVI profile plots


#-------------------------------------------------------------------------------
print("3 | Deriving samples for classification applying a comparison of pure sample NDVI profiles and pixel-specific NDVI profiles")
#-------------------------------------------------------------------------------
###Additional parameters
#ZS.SHP - shape file with DOY-specific NDVI values; result from applying fSample function 

###Function
SAMPLE <- fSample(W.DIR,
                  PS,
                  IN.DIR = "_data/",
                  ZS.SHP = paste(RU.SHP,YEAR,sep=""),
                  RU.SHP = RU.SHP,
                  OUT.DIR = "_result/",
                  Q = 1)

print(SAMPLE$SAMPLE.SHP)
print(SAMPLE$SAMPLE.AGG)

### B | result
#RU_fergana[YEAR]_SAMPLE-NDVI_Q[Q].shp -> shape file of detected samples
#RU_fergana[YEAR]_SAMPLE-NDVI_agg_Q[Q].csv -> csv file of aggregated sample NDVI profiles


#-------------------------------------------------------------------------------
print("3 | Comparison of Pure Samples and aggregated samples derived with fSample function")
#-------------------------------------------------------------------------------
###Additional parameters
#column name with class names
CLASS.NAME <- "CLASS"
#name of pure sample file
PS1 <- PS
#name of file containing aggregated sample NDVI profiles
PS2 <- SAMPLE$SAMPLE.AGG
#prefix of columns containing DOY-specific NDVI values within PS1
PS1PF <- "PS"
#prefix of columns containing DOY-specific NDVI values within PS2
PS2PF <- "MD"
#threshold which proportion of class-specific NDVI values should be considered (1=100%)  
TH <- 0.75

###Function
fPuSaCompare(W.DIR,
             IN.DIR,
             OUT.DIR,
             CLASS.NAME,
             PS1,
             PS2,
             PS1PF,
             PS2PF,
             TH=0.75)

### C | result
#PureSample__RU_fergana[YEAR]_SAMPLE-NDVI_agg_Q[Q].pdf - plot of NDVI profiles based on both pure samples and aggregated samples derived with fPuSaDi function

#-------------------------------------------------------------------------------
print("5 | MODIS classification")
#-------------------------------------------------------------------------------
###Function
MODIS.CLASS <- fModisClass(W.DIR,
                           IN.DIR = OUT.DIR,
                           OUT.DIR,
                           ZS.SHP = RU.MODIS,
                           SAMPLE.SHP = SAMPLE$SAMPLE.SHP,
                           PART = 0.75,
                           T.CLASS = "CLASS",
                           M.TRAIN = "rf",
                           UpSample = TRUE)

