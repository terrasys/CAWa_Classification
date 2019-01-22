print("Derivation of samples")
#-----------------------------------------------------------------------------------------------------
fSample <- function(W.DIR,
                    IN.DIR,
                    PS,
                    ZS.SHP,
                    RU.SHP,
                    OUT.DIR,
                    Q){
  print("Sampling: Import pure sample data set")
  #-----------------------------------------------------------------------------------------------------
  #Import pure samples
  #-----------------------------------------------------------------------------------------------------
  setwd(W.DIR)
  ps <- read.table(paste(IN.DIR,PS,sep=""),
                   dec=",",
                   sep=";",
                   header = TRUE,
                   check.names=FALSE)
  
  #Sort class names 
  ps <- ps[order(ps$CLASS),]
  #Add a CLASS_ID column 
  ps <- data.frame(CLASS_ID = 1:nrow(ps),ps)
  print(head(ps))
  #-----------------------------------------------------------------------------------------------------
  print("Import MODIS data")
  #-----------------------------------------------------------------------------------------------------
  print(getwd())
  df.md <-  read.dbf(paste(W.DIR,OUT.DIR,ZS.SHP,".dbf",sep=""))$dbf
  #replace all values <0
  head(df.md)
  df.md[df.md<0] <- 0
  #remove NA
  df.md <- na.omit(df.md)
  print(head(df.md))
  #-----------------------------------------------------------------------------------------------------
  print("Sampling: Comparing pure samples with MODIS data")
  #-----------------------------------------------------------------------------------------------------
  #beginCluster( detectCores() -1) #use all but one core  
  pb <- txtProgressBar(min=min(ps$CLASS_ID), max=length(ps$CLASS_ID), style=3)
  for(j in ps$CLASS_ID){
    d.cor <- sapply(1:nrow(df.md), function(i) diss.COR(as.numeric(df.md[i,grepl(paste("MD",sep=""), names(df.md))]), as.numeric(ps[j,grepl(paste("PS",sep=""), names(ps))])))
    d.cort <- sapply(1:nrow(df.md), function(i) diss.CORT(as.numeric(df.md[i,grepl(paste("MD",sep=""), names(df.md))]), as.numeric(ps[j,grepl(paste("PS",sep=""), names(ps))])))
    d <- featureScale(d.cor*d.cort)
    d <- data.frame(d)
    colnames(d) <- ps[which(ps$CLASS_ID==j),]$CLASS
    print(head(d))
    #print(getwd)
    #
    write.table(d, 
                file = paste(W.DIR,OUT.DIR,"d",ps[which(ps$CLASS_ID==j),]$CLASS,c(".csv"),sep=""), 
                sep = ";", 
                dec=",",
                row.names = FALSE)
    setTxtProgressBar(pb, j)
  }
  #endCluster()
  #-----------------------------------------------------------------------------------------------------
  #Summarize all class-specific distance files
  #-----------------------------------------------------------------------------------------------------
  setwd(file.path(W.DIR,OUT.DIR))
  d.l <- list.files(pattern="^(d).*\\.csv$")
  D <- data.frame(ID=df.md$ID)
  for(i in d.l){
    d <- read.table(file.path(paste(W.DIR,OUT.DIR,i,sep="")),
                    dec=",",
                    sep=";",
                    header = TRUE,
                    check.names=FALSE)
    D <- cbind(D,d)
  }
  #-----------------------------------------------------------------------------------------------------
  #export comparison results (Dissimilarity Matrix)
  #-----------------------------------------------------------------------------------------------------
  write.table(D,
              file=paste(W.DIR,OUT.DIR,ZS.SHP,"_DM",c(".csv"),sep=""),
              dec=",",
              sep=";",
              row.names = FALSE)
  #-----------------------------------------------------------------------------------------------------
  print("Select quantile-specific samples")
  #-----------------------------------------------------------------------------------------------------
  for(i in as.numeric(ps$CLASS)){
    D[[paste("S",i,sep="")]] <- ifelse(D[[paste(i)]]<=quantile(D[[paste(i)]],probs = seq(0.01,1,0.01))[[Q]],
                                       1,
                                       0)
  }
  #-----------------------------------------------------------------------------------------------------
  #Selection of all data sets which fulfil the quantile condition
  #-----------------------------------------------------------------------------------------------------
  #Empty data frame
  df.S <- data.frame(matrix(ncol=(length(df.md)+2)))[-1,]
  for(i in as.numeric(ps$CLASS)){
    x <- data.frame(ID_R=D$ID,D[paste("S",i,sep="")])
    x <- x[which(x[paste("S",i,sep="")]==1),]
    x$CLASS <- i
    x <- merge(x,df.md,by="ID_R")
    colnames(df.S) <- names(x) 
    df.S <- rbind(df.S,x)
  }
  #Detect and delete dublicates
  df.S <- df.S[!duplicated(df.S$ID), ]
  head(df.S)
  #Barplot of sample number
  setwd(file.path(W.DIR,OUT.DIR))
  pdf(paste(ZS.SHP,"_SAMPLE-NDVI-barplot_Q",Q,c(".pdf"),sep=""),width=10,height=4)
  xx <- barplot(table(df.S$CLASS)/1000,
                ylab="Number x 1000",
                sub=paste('Total number =',sum(table(df.S$CLASS))),
                xlab="CROP TYPE CODE",
                las=1,
                ylim=c(0,(max(table(df.S$CLASS))/1000)),
                main=paste("Sample number =",sum(table(df.S$CLASS))))
  text(x = xx, y = 0, label = table(df.S$CLASS), pos = 3, cex = 0.8, col = "red")
  dev.off()
  #------------------------------------------------------------------------------------------------------
  print("Sampling: Export") 
  #------------------------------------------------------------------------------------------------------
  setwd(file.path(W.DIR,OUT.DIR))
  write.table(df.S,
              file=paste(ZS.SHP,"_SAMPLE-NDVI_Q",Q,c(".csv"),sep=""),
              dec=",",
              sep=";",
              row.names = FALSE)
  #Aggregation
  df.S.agg <- data.frame(CLASS=names(split(df.S,df.S$CLASS)))
  df.temp <- data.frame(CLASS=df.S$CLASS,df.S[grepl(paste("MD",sep=""), names(df.S))])
  for(i in names(df.temp[grepl(paste("MD",sep=""), names(df.temp))])){
    x <- aggregate(df.temp[[paste(i)]], 
                   by=list(df.temp$CLASS),
                   FUN=mean, 
                   na.rm=TRUE)
    colnames(x) <- c("CLASS",paste(i))
    df.S.agg <- merge(df.S.agg,x,by="CLASS")
  }
  #Export aggregated samples") 
  setwd(file.path(W.DIR,OUT.DIR))
  write.table(df.S.agg,
              file=paste(ZS.SHP,"_SAMPLE-NDVI_agg_Q",Q,c(".csv"),sep=""),
              dec=",",
              sep=";",
              row.names = FALSE)
  #Export sample shape file"
  #Import shape file of MODIS raster polygones
  rus <- st_read(paste(W.DIR,IN.DIR,RU.SHP,".shp",sep=""))
  head(rus)
  #Merge with sample data frame
  rus <- merge(rus,df.S,
               by="ID_R",
               all = TRUE)
  #NA replacement 
  rus[is.na(rus)] <- -9999
  #select polygons with D>=0
  rus.s <- rus[rus$"CLASS">=0,]
  #Export 
  setwd(file.path(W.DIR,OUT.DIR))
  st_write(rus.s,paste(ZS.SHP,"_SAMPLE-NDVI_Q",Q,".shp",sep=""),delete_layer = TRUE)
  return(list(SAMPLE.AGG = paste(RU.SHP,YEAR,"_SAMPLE-NDVI_agg_Q",Q,".csv",sep=""),
              SAMPLE.SHP = paste(RU.SHP,YEAR,"_SAMPLE-NDVI_Q",Q,sep="")))
}