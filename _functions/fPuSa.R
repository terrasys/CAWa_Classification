print("Dissimiliariy test of samples")
#-----------------------------------------------------------------------------------------------------
#Dissimiliariy test of pure samples
#-----------------------------------------------------------------------------------------------------
fPuSa <- function(W.DIR,
                  IN.DIR,
                  OUT.DIR,
                  CLASS.NAME,
                  PS){
#select directory with sample profiles
print("Dissimiliariy test of samples")
#-----------------------------------------------------------------------------------------------------
print(paste("A | Import pure samples:",PS))
#-----------------------------------------------------------------------------------------------------
ps <- read.table(file.path(W.DIR,IN.DIR,PS),
                 dec=",",
                 sep=";",
                 header = TRUE,
                 check.names=FALSE)
ps <- ps[order(ps[paste(CLASS.NAME)]),]
#-------------------------------------------------------------------------------
print("B | Plot NDVI profiles")
#-------------------------------------------------------------------------------
#split data set according to class
l.class <- split(ps,ps[[paste(CLASS.NAME)]])
#color schema
my.palette <- brewer.pal(n = length(l.class), name = "Spectral")
setwd(file.path(W.DIR,OUT.DIR))
pdf(paste(substr(PS,1,nchar(PS)-4),"_NDVI-profiles.pdf",sep=""), 
    height=5,width=9)
  DOY <- data.frame(DOY=-25:410,NDVI=-1)
  plot(DOY,
       xaxt="n",
       ylim=c(0,1),
       xlim=c(0,380),
       ylab=as.expression(bquote(italic(NDVI))),
       xlab=as.expression(bquote(italic(DOY))),
       cex.lab=1.4)
  #axis
  x1 <- seq(1,365,10)
  x2 <- seq(1,360,2)
  axis(1, at=x2, col.tick="grey", las=1,labels=FALSE,cex=1.2)
  axis(1, at=x1, col.axis="black", las=1,cex=1.2)
  for(c in 1:length(l.class)){
  #select parcel-specific row  
  ndvi <- l.class[[c]] 
  #extract all columns containing LS in column name
  ndvi <- ndvi[grepl(paste("PS",sep=""), names(ndvi))]
  #replace all values smaller 0 with 0
  ndvi[ndvi<0] <- 0
  #interpolate  NA values if they exist
  if(sum(is.na(ndvi[1,]))>0){
    ndvi[1,] <- na.spline(c(ndvi))
  }
  #empty data frames 
  v.doy <- data.frame(DOY=NULL)
  v.ndvi <- data.frame(NDVI=NULL)
  for(j in 1:length(ndvi)){
    points(noquote(substr(names(ndvi[j]),3,6)),ndvi[1,j])
    v.doy <- rbind(v.doy,as.numeric(noquote(substr(names(ndvi[j]),3,6))))
    v.ndvi <- rbind(v.ndvi,ndvi[1,j])
  }
  ks <- spline(v.doy[[1]],v.ndvi[[1]], method='n', n=length(ndvi)*10)
  lines(ks, col=my.palette[c],lwd=2,lty=5)
  }
  legend("top",
         legend=c(paste(as.character(ps[[paste(CLASS.NAME)]]))),
         lty=5,
         lwd=2,
         cex=1.2,
         col=my.palette,
         bty="n",ncol=length(l.class))
dev.off()
#------------------------------------------------------------------------------------------------------
print("C | Dissimilarity test")
#------------------------------------------------------------------------------------------------------
#Melt data so that each row is a unique id-variable combination
ps[[paste(CLASS.NAME)]] <- as.factor(ps[[paste(CLASS.NAME)]])
ps.m <- melt(ps)
ps.m
#Split melted data set
ps.m.s <- split(ps.m,ps.m[paste(CLASS.NAME)])
#vector of available classes
c.l <- t(ps)[1,]
#Contigency table of dissimilarity test results
d.cor <- data.frame(D.cor=NULL)
d.cort <- data.frame(D.cort=NULL)
b <-  data.frame(CLASS1=as.character())
c <-  data.frame(CLASS2=as.character())
#Compare class-specific NDVI values
pb <- txtProgressBar(min=1, max=length(c.l), style=3)
for(i in c.l){
  for(j in c.l){
    x <- diss.COR(ps.m.s[[i]]$value,ps.m.s[[j]]$value)
    y <- diss.CORT(ps.m.s[[i]]$value,ps.m.s[[j]]$value)
    d.cor <- rbind(d.cor,x)
    d.cort <- rbind(d.cort,y)
    b <- rbind(b,as.numeric(i))
    c <- rbind(c,as.numeric(j))
  }
setTxtProgressBar(pb, i)  
}
d <- data.frame(d.cor,d.cort,b,c)
colnames(d) <- c("D.COR","D.CORT","CLASS1","CLASS2")
#Scaled product of"D.COR" and "D.CORT"
d$D <- featureScale(d$D.COR*d$D.CORT)
#Split comparison results accordint to classes
d.st <- split(d,d$CLASS1)
#Combine 
d.fs <- data.frame(c.l)
for(i in c.l){
  d.fs <- cbind(d.fs,d.st[[i]][5])
}
colnames(d.fs) <- c("CLASS",c.l)
d.fs
#Export dissimilarity test results
setwd(file.path(W.DIR,OUT.DIR))
write.csv2(d.fs,
            file=paste(substr(PS,1,nchar(PS)-4),"_DM",c(".csv"),sep=""),
            row.names = FALSE)
}
