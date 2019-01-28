print("Function for zonal statistics of statistical imagery")
#-------------------------------------------------------------------------------
fZonaS <- function(
  W.DIR,
  IN.DIR,
  RU.SHP,
  OUT.DIR,
  YEAR){
#-----------------------------------------------------------------------------------------------------
print("Import reference units")
#-----------------------------------------------------------------------------------------------------
ru <- shapefile(paste(W.DIR,IN.DIR,RU.SHP,".shp",sep=""))
#save column names of shape file
names.ru <- names(ru@data)
#-----------------------------------------------------------------------------------------------------
print("Import MODIS raster")
#-----------------------------------------------------------------------------------------------------
setwd(file.path(W.DIR,IN.DIR))
l.r <- mixedsort(list.files(pattern=paste("^(",YEAR,").*\\.tif$",sep="")),decreasing=TRUE)
#convert dates into DOY
DOY <- data.frame(DOY=NULL)
for(i in 1:length(l.r)){
  d <- date2jul2(substr(l.r,9,10)[i], substr(l.r,6,7)[i],YEAR)$doy
  DOY <- rbind(DOY,d)
}
colnames(DOY) <- c("DOY")
for(i in 1:nrow(DOY)){
  if(nchar(DOY[i,])==2){DOY[i,] <-  paste("0",DOY[i,],sep="")}
  if(nchar(DOY[i,])==1){DOY[i,] <- paste("00",DOY[i,],sep="")}
}
#create a layerstack
m <- stack(l.r)
#Sisosoidale projection
#proj4string(m) <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#-----------------------------------------------------------------------------------------------------
print("Reproject RU according to satellite imagery")
#-----------------------------------------------------------------------------------------------------
#reproject ru accordung to m
ru <- spTransform(ru, m@crs)
#export shape file
shapefile(ru, paste(W.DIR,OUT.DIR,RU.SHP,sep=""), overwrite=TRUE)
#-----------------------------------------------------------------------------------------------------
print("Transform imagery to SAGA format")
#-----------------------------------------------------------------------------------------------------
pb <- txtProgressBar(min=0, max=length(m@layers), style=3)  
for(i in 1:nrow(DOY)){
  writeRaster(m[[i]],
              paste("NDVI",DOY[i,],sep=""),
              format="SAGA",
              overwrite=TRUE)
  setTxtProgressBar(pb, i)
}
#-----------------------------------------------------------------------------------------------------
print("Zonal statistic")
#-----------------------------------------------------------------------------------------------------
setwd(file.path(W.DIR,IN.DIR))
l.r <- list.files(pattern="*.*\\.sgrd$")
l.r <- mixedsort(l.r[grep(paste("NDVI",sep=""), l.r)])
pb <- txtProgressBar(min=0, max=length(l.r), style=3)
for (i in 1:length(l.r)){
  rsaga.geoprocessor(
    lib="shapes_grid",
    module=2,
    param=list(GRIDS=file.path(W.DIR,IN.DIR,l.r[i]),
               POLYGONS=paste(W.DIR,OUT.DIR,RU.SHP,".shp",sep=""),
               COUNT=0,
               MEAN=0,
               MIN=0,
               MAX=1,
               RANGE=0,
               SUM=0,
               VAR=0,
               STDDEV=0,
               QUANTILE=0,
               NAMING=0),
    env=myenv)
  setTxtProgressBar(pb, i)
}
print("Import and rename attributed shape file attributes")
ru <- st_read(paste(W.DIR,OUT.DIR,RU.SHP,".shp",sep=""))
colnames(ru) <- c(names.ru,paste("MD",DOY$DOY,sep=""),paste("geometry"))
st_write(ru,paste(W.DIR,OUT.DIR,RU.SHP,YEAR,".shp",sep=""),delete_layer = TRUE)
return(paste(RU.SHP,YEAR,sep=""))
}
