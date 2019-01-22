featureScale <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
#-----------------------------------------------------------------------------------------------------
#Dissimiliariy test of pure samples
#-----------------------------------------------------------------------------------------------------
fPuSaCompare <- function(W.DIR,
                 IN.DIR,
                 OUT.DIR,
                 CLASS.NAME,
                 PS1,
                 PS2,
                 PS1PF,
                 PS2PF,
                 TH){
#select directory with sample profiles
#-----------------------------------------------------------------------------------------------------
print(paste("Visual comparison of two pure sample sets:",PS1,"and",PS2))
#-----------------------------------------------------------------------------------------------------
ps1 <- read.table(file.path(W.DIR,IN.DIR,PS1),
                 dec=",",
                 sep=";",
                 header = TRUE,
                 check.names=FALSE)
ps1 <- ps1[order(ps1[paste(CLASS.NAME)]),]
ps2 <- read.table(file.path(W.DIR,OUT.DIR,PS2),
                  dec=",",
                  sep=";",
                  header = TRUE,
                  check.names=FALSE)
ps2 <- ps2[order(ps2[paste(CLASS.NAME)]),]
ps1
ps2
#-------------------------------------------------------------------------------
print("Plot NDVI profiles")
#-------------------------------------------------------------------------------
#split data set according to class
l1.class <- split(ps1,ps1[[paste(CLASS.NAME)]])
l2.class <- split(ps2,ps2[[paste(CLASS.NAME)]])
#plot
  setwd(file.path(W.DIR,OUT.DIR))
  pdf(paste(substr(PS1,1,nchar(PS1)-4),"__",substr(PS2,1,nchar(PS2)-4),".pdf",sep=""), 
      height=5,width=9)
  for(c in 1:length(l1.class)){
  #select parcel-specific row  
  ndvi1 <- l1.class[[c]]
  ndvi2 <- l2.class[[c]]
  #plot NDVI profiles when the number of NDVI values exeeds threshold TH
    DOY <- data.frame(DOY=-25:410,NDVI=-1)
    plot(DOY,
         xaxt="n",
         ylim=c(0,1),
         xlim=c(0,380),
         ylab=as.expression(bquote(italic(NDVI))),
         xlab=as.expression(bquote(italic(DOY))),
         cex.lab=1.4,
         main=paste("CLASS-ID =",l1.class[[c]]$CLASS))
    #axis
    x1 <- seq(1,365,10)
    x2 <- seq(1,360,2)
    axis(1, at=x2, col.tick="grey", las=1,labels=FALSE,cex=1.2)
    axis(1, at=x1, col.axis="black", las=1,cex=1.2)
    #extract all columns containing LS in column name
    ndvi1 <- ndvi1[grepl(paste(PS1PF,sep=""), names(ndvi1))]
    ndvi2 <- ndvi2[grepl(paste(PS2PF,sep=""), names(ndvi2))]
    #replace all values smaller 0 with 0
    ndvi1[ndvi1<0] <- 0
    ndvi2[ndvi2<0] <- 0
    #interpolate  NA values if they exist
    if(sum(is.na(ndvi1[1,]))>0){
      ndvi1[1,] <- na.spline(c(ndvi1))
    }
    if(sum(is.na(ndvi2[1,]))>0){
      ndvi2[1,] <- na.spline(c(ndvi2))
    }
    #empty data frames for sample 1
    v.doy <- data.frame(DOY=NULL)
    v.ndvi <- data.frame(NDVI=NULL)
    for(j in 1:length(ndvi1)){
      points(noquote(substr(names(ndvi1[j]),3,6)),ndvi1[1,j])
      v.doy <- rbind(v.doy,as.numeric(noquote(substr(names(ndvi1[j]),3,6))))
      v.ndvi <- rbind(v.ndvi,ndvi1[1,j])
    }
    ks <- spline(v.doy[[1]],v.ndvi[[1]], method='n', n=length(ndvi1)*10)
    lines(ks, col="red",lwd=1.5,lty=5)
    
    #empty data frames for sample 2
    v.doy <- data.frame(DOY=NULL)
    v.ndvi <- data.frame(NDVI=NULL)
    for(j in 1:length(ndvi2)){
      points(noquote(substr(names(ndvi2[j]),3,6)),ndvi2[1,j])
      v.doy <- rbind(v.doy,as.numeric(noquote(substr(names(ndvi2[j]),3,6))))
      v.ndvi <- rbind(v.ndvi,ndvi2[1,j])
    }
    ks <- spline(v.doy[[1]],v.ndvi[[1]], method='n', n=length(ndvi2)*10)
    lines(ks, col="blue",lwd=2,lty=5)
    
    #lgend
    legend("topleft", 
           legend = c(paste(PS1), paste(PS2)), 
           col = c("red","blue"), 
           lty = c(5,5),
           lwd=c(2,2),
           bty = "y", 
           pt.cex = 2, 
           cex = 1.2, 
           text.col = "black")
  }
  dev.off()
}
