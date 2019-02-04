#Wrapper file excecuting all functions for the MDOIS classification
#-----------------------------------------------------------------------------------------------------
print("Working directory, data and settings")
#-----------------------------------------------------------------------------------------------------
#working directory
W.DIR <- "d:/Dropbox/_git/CAWaClass/"
#directory containing functions
FUNC.DIR <- "_functions/"
#directory containing input data
IN.DIR <- "_data/"
#directory containing results
OUT.DIR <- "_result/"
#name of reference unit shapefile
MODIS.SHP = "MODIS_fergana"
#Irrigation mask
IM.GRD = "IM.sgrd"
#name of pure sample file
PS  = "PureSample.csv"
YEAR = 2015
#-----------------------------------------------------------------------------------------------------
print("Import functions")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackage.R"))
source(file.path(W.DIR,FUNC.DIR,"fPuSa.R"))
source(file.path(W.DIR,FUNC.DIR,"fZonaSt.R"))
source(file.path(W.DIR,FUNC.DIR,"fSample.R"))
source(file.path(W.DIR,FUNC.DIR,"fPuSaCom.R"))
source(file.path(W.DIR,FUNC.DIR,"fModisClass.R"))
source(file.path(W.DIR,FUNC.DIR,"fClassAcc.R"))
#-----------------------------------------------------------------------------------------------------
print("Carry out functions")
#-----------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
print("Dissimiliarity test of samples")
#-------------------------------------------------------------------------------
###Function
fPuSa(W.DIR=W.DIR,
      IN.DIR=IN.DIR,
      OUT.DIR=OUT.DIR,
      CLASS.NAME = "CLASS",
      PS=PS)

#-------------------------------------------------------------------------------
print("Zonal statistics of reference units for MODIS imagery")
#-----------------------------------------------------------------------------------------------------
###Function
ZS.SHP <- fZonaSt(W.DIR=W.DIR,
                   IN.DIR=IN.DIR,
                   OUT.DIR=OUT.DIR,
                   MODIS.SHP=MODIS.SHP,
                   IM.GRD,
                   YEAR = YEAR)

print(ZS.SHP)
###Result
#RU_fergana[YEAR].shp -> shape file with DOY-specific NDVI values 
#PureSample_NDVI-profiles.pdf -> class-specific NDVI profile plots


#-------------------------------------------------------------------------------
print("Deriving samples for classification applying a comparison of pure sample NDVI profiles and pixel-specific NDVI profiles")
#-------------------------------------------------------------------------------
###Additional parameters
#ZS.SHP - shape file with DOY-specific NDVI values; result from applying fSample function 

###Function
SAMPLE <- fSample(W.DIR=W.DIR,
                  PS=PS,
                  IN.DIR = IN.DIR,
                  ZS.SHP = ZS.SHP,
                  MODIS.SHP = MODIS.SHP,
                  OUT.DIR = OUT.DIR,
                  Q = 1)

print(SAMPLE$SAMPLE.SHP)
print(SAMPLE$SAMPLE.AGG)

### B | result
#RU_fergana[YEAR]_SAMPLE-NDVI_Q[Q].shp -> shape file of detected samples
#RU_fergana[YEAR]_SAMPLE-NDVI_agg_Q[Q].csv -> csv file of aggregated sample NDVI profiles


#-------------------------------------------------------------------------------
print("3 | Comparison of Pure Samples and aggregated samples derived with fSample function")
#-------------------------------------------------------------------------------
###Function
fPuSaCom(W.DIR=W.DIR,
         IN.DIR=IN.DIR,
         OUT.DIR=OUT.DIR,
         CLASS.NAME="CLASS",
         PS1=PS,
         PS2=SAMPLE$SAMPLE.AGG,
         PS1PF="PS",
         PS2PF="MD",
         TH=0.75)

### C | result
#PureSample__RU_fergana[YEAR]_SAMPLE-NDVI_agg_Q[Q].pdf - plot of NDVI profiles based on both pure samples and aggregated samples derived with fPuSaDi function

#-------------------------------------------------------------------------------
print("5 | MODIS classification")
#-------------------------------------------------------------------------------
###Function
MODIS.CLASS <- fModisClass(W.DIR=W.DIR,
                           IN.DIR = OUT.DIR,
                           OUT.DIR=OUT.DIR,
                           ZS.SHP = ZS.SHP,
                           SAMPLE.SHP = SAMPLE$SAMPLE.SHP,
                           PART = 0.75,
                           T.CLASS = "CLASS",
                           M.TRAIN = "rf",
                           UpSample = TRUE)

