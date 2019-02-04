print("MODIS classification script based on PuSa samples")
#-----------------------------------------------------------------------------------------------------
fModisClass <- function(W.DIR,
                       IN.DIR,
                       OUT.DIR,
                       SAMPLE.SHP,
                       ZS.SHP,
                       PART,
                       T.CLASS,
                       M.TRAIN,
                       UpSample=TRUE){
#-----------------------------------------------------------------------------------------------------
print("Import samples derived from applying fSample function")
#-----------------------------------------------------------------------------------------------------
s <- shapefile(file.path(W.DIR,OUT.DIR,SAMPLE.SHP))
#------------------------------------------------------------------------------------------------------
print("Data partition using createDataPartition() function")
#------------------------------------------------------------------------------------------------------
#Splitting data as training and test set. Using createDataPartition() function from caret
set.seed(123)
indxTrain <- createDataPartition(y = s[[c(c(match(paste(T.CLASS,sep=""),names(s))))]],p = PART,list = FALSE)
training <- s@data[indxTrain,]
testing <- s@data[-indxTrain,]
#Checking distibution in original data and partitioned data
training <- na.omit(training)
testing <- na.omit(testing)
#comparison of sample fractions between training and testing  
setwd(file.path(W.DIR,OUT.DIR))
pdf(paste(SAMPLE.SHP,"_BARPLOT-SAMPLE-training-testing",c(".pdf"),sep=""),width=7,height=10)
par(mfrow = c(2, 1))
#barplot (percentage)
n.tab <- table(training$CLASS)
xx <- barplot(n.tab/sum(n.tab)*100,
              ylab="Proportion [%]",
              sub=paste('Total number =',sum(n.tab)),
              xlab="CLASS",
              las=2,
              ylim=c(0,30),
              main="Training")
text(x = xx, y = 0, label = round(n.tab/sum(n.tab)*100,0), pos = 3, cex = 0.8, col = "red")
n.tab <- table(testing$CLASS)
xx <- barplot(n.tab/sum(n.tab)*100,
              ylab="Proportion [%]",
              sub=paste('Total number =',sum(n.tab)),
              xlab="CLASS",
              las=2,
              ylim=c(0,30),
              main="Test")
text(x = xx, y = 0, label = round(n.tab/sum(n.tab)*100,0), pos = 3, cex = 0.8, col = "red")
dev.off()
#Subsetting data set for modelling 
training <- data.frame(T.CLASS=training[paste(T.CLASS)],training[grepl(paste("MD",sep=""), names(training))])
training <- na.omit(training)
head(training)
#------------------------------------------------------------------------------------------------------
print("UpSampling with upSample() function")
#------------------------------------------------------------------------------------------------------
if(UpSample==TRUE){
training$CLASS <- as.factor(training$CLASS)
up_train <- upSample(training,training$CLASS)
training <- up_train[c(1:(length(up_train)-1))]
}
#------------------------------------------------------------------------------------------------------
print("Training")
#------------------------------------------------------------------------------------------------------
set.seed(123)
#Configure parallel processing
ctrl <- trainControl(method="repeatedcv",
                     number=10,
                     classProbs=FALSE,
                     savePredictions = TRUE,
                     allowParallel = FALSE)

#cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
#registerDoParallel(cluster)
m.Fit <-   train(as.factor(CLASS) ~ .,
                 data = training,
                 method = M.TRAIN,
                 trControl = ctrl,
                 preProc = c("center", "scale"),
                 importance = TRUE,
                 verbose = TRUE)
#stopCluster(cluster)
#registerDoSEQ()
##print crossvalidation results 
setwd(file.path(W.DIR,OUT.DIR))
sink(paste(SAMPLE.SHP,"_",M.TRAIN,"_CV",c(".txt"),sep=""))
print(m.Fit)
sink()
#------------------------------------------------------------------------------------------------------
print("Prediction to the test data set")
#------------------------------------------------------------------------------------------------------
##prediction to the test data set
testing$CLASS_sim <- predict(m.Fit,testing)
#calculating export accuracy metrics
set.seed(777)
acc.m <- fClassAcc(actual=testing$CLASS, predicted=testing$CLASS_sim)
setwd(file.path(W.DIR,OUT.DIR))
write.table(acc.m$ConfusionMatrix, 
            file = paste(SAMPLE.SHP,"_",M.TRAIN,"_CM",c(".csv"),sep=""), 
            sep = ";", 
            dec=",")
write.table(acc.m$AccMetrics, 
            file = paste(SAMPLE.SHP,"_",M.TRAIN,"_AM",c(".csv"),sep=""), 
            sep = ";", 
            dec=",")
#------------------------------------------------------------------------------------------------------
print("Prediction to the total data set")
#------------------------------------------------------------------------------------------------------
#Import reference units)
m <- st_read(paste(W.DIR,IN.DIR,ZS.SHP,".shp",sep=""))
#Removing NA in shape file 
m[is.na(m)] <- -9999
m <- m[m[[c(3)]]>=0,]
#prediction to the total data set
m$CLASS <- predict(m.Fit,m)
#deriving probabilities
m$CLASS_PB <- apply(predict(m.Fit,m,type="prob"), 1, max)
#export
setwd(file.path(W.DIR,OUT.DIR))
st_write(m,paste(ZS.SHP,"_",M.TRAIN,"_CLASS.shp",sep=""),delete_layer = TRUE)
#------------------------------------------------------------------------------------------------------
print("Barplot of classification results")
#------------------------------------------------------------------------------------------------------
setwd(file.path(W.DIR,OUT.DIR))
pdf(paste(ZS.SHP,"_",M.TRAIN,"_classification_barplot",c(".pdf"),sep=""), height=4,width=10)
#barplot (percentage)
n.tab <- table(m$CLASS)
xx <- barplot(n.tab/sum(n.tab)*100,
              ylab="Proportion [%]",
              sub=paste('Total number =',sum(n.tab)),
              xlab="CLASS",
              las=2,
              ylim=c(0,50),
              main=paste("Classification (",M.TRAIN,")",sep=""))
text(x = xx, y = 0, label = round(n.tab/sum(n.tab)*100,0), pos = 3, cex = 0.8, col = "red")
dev.off()
return(MODIS.CLASS = paste(ZS.SHP,"_",M.TRAIN,"_CLASS",sep=""))
}
