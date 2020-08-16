library(lubridate)
library(data.table)
library(randomForest)
library(caret)
library(stringr)
library(caret)
library(ranger)
library(CAST)
library(raster)

a <- seq(9,13,1)
a <- sprintf("%02d", a)
library(CAST)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)


for (i in a){

  mintemppath <- paste0("C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/tidy_mintemp/mintempcol_20",i,".csv")
  
  mintemp<-fread(mintemppath)
  mintemp<-mintemp[,-c(2,3)]
  mintemp<-reshape2::melt(mintemp,id.vars='ID')
  colnames(mintemp)[1:3]<-c('ID','date','at')
  
  mintemp$date<-as.Date(mintemp$date,"%Y/%m/%d")
  mintemp<-transform(mintemp,Year=as.character(format(as.Date(date),"%Y")))
  mintemp<-transform(mintemp,Month=as.character(format(as.Date(date),"%m")))
  mintemp$id_month<-paste0(mintemp$ID,'_',mintemp$Month)
  
  monthmintemp <- aggregate(at~id_month+ID,mintemp,FUN=mean)
  
  
  #maxtemppath <- paste0("C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/tidy_maxtemp/maxtempcol_20",i,".csv")
  #
  #maxtemp<-fread(maxtemppath)
  #maxtemp<-maxtemp[,-c(2,3)]
  #maxtemp<-reshape2::melt(maxtemp,id.vars='ID')
  #colnames(maxtemp)[1:3]<-c('ID','date','at')
  #
  #maxtemp$date<-as.Date(maxtemp$date,"%Y/%m/%d")
  #maxtemp<-transform(maxtemp,Year=as.character(format(as.Date(date),"%Y")))
  #maxtemp<-transform(maxtemp,Month=as.character(format(as.Date(date),"%m")))
  #maxtemp$id_month<-paste0(maxtemp$ID,'_',maxtemp$Month)
  #
  #monthmaxtemp <- aggregate(at~id_month+ID,maxtemp,FUN=mean)
  #
  #monthmaxtemp$avt <- (monthmaxtemp$at+monthmintemp$at)/2
  #monthmeantemp    <- monthmaxtemp[,-c(3)]
  
  indicespath <- paste0('C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/resample_csv/',i,'_Indices.csv')
  
  indices<-fread(indicespath)
  indices<-indices[,-c(2,3,4)]
  indices<-reshape2::melt(indices,id.vars='ID')
  indices$month<-substr(indices$variable,3,4)
  indices$name <- substr(indices$variable,6,length(indices$variable))
  indices <- data.frame(indices)
  indices <- indices[,-c(2)]
  indices<-reshape2::dcast(indices, ID+month~name)
  indices$id_month<-paste0(indices$ID,'_',indices$month)
  
  srpath <- paste0('C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/resample_csv/',i,'_MSR.csv')
  
  sr<-fread(srpath)
  sr<-sr[,-c(2,3,4)]
  sr<-reshape2::melt(sr,id.vars='ID')
  sr$month<-substr(sr$variable,3,4)
  sr <- data.frame(sr)
  sr <- sr[,-c(2)]
  sr$id_month<-paste0(sr$ID,'_',sr$month)
  sr <- sr[c('ID','month','value','id_month')]
  colnames(sr) <- c('ID','month','sr','id_month')
  
  LSTdaypath <- paste0('C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/resample_csv/',i,'_Tmax.csv')
  LSTday<-fread(LSTdaypath)
  LSTday<-LSTday[,-c(2,3,4)]
  LSTday<-reshape2::melt(LSTday,id.vars='ID')
  LSTday$month<-substr(LSTday$variable,3,4)
  LSTday <- data.frame(LSTday)
  LSTday <- LSTday[,-c(2)]
  LSTday$id_month<-paste0(LSTday$ID,'_',LSTday$month)
  LSTday <- LSTday[c('ID','month','value','id_month')]
  colnames(LSTday) <- c('ID','month','LSTday','id_month')
  
  
  LSTnightpath <- paste0('C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/resample_csv/',i,'_Tmin.csv')
  LSTnight<-fread(LSTnightpath)
  LSTnight<-LSTnight[,-c(2,3,4)]
  LSTnight<-reshape2::melt(LSTnight,id.vars='ID')
  LSTnight$month<-substr(LSTnight$variable,3,4)
  LSTnight <- data.frame(LSTnight)
  LSTnight <- LSTnight[,-c(2)]
  LSTnight$id_month<-paste0(LSTnight$ID,'_',LSTnight$month)
  LSTnight <- LSTnight[c('ID','month','value','id_month')]
  colnames(LSTnight) <- c('ID','month','LSTnight','id_month')
  
  eles<-fread('D:/SCI_Paper/Paper2/Data/tp_tci.csv')
  id_latlong<-eles[,c(1:3)]
  ele<-eles[,c(1,4)]
  #tci <- eles[,c(1,5)]
  
  rf_minput<-monthmintemp
  
  rf_minput<-merge(rf_minput,ele,by='ID',all.x = T)
  
  #rf_minput<-merge(rf_minput,tci,by='ID',all.x = T)
  
  rf_minput<-merge(rf_minput,sr[,3:4],by='id_month',all.x = T)
  
  rf_minput<-merge(rf_minput,LSTday[,2:4],by='id_month',all.x = T)
  
  rf_minput<-merge(rf_minput,LSTnight[,3:4],by='id_month',all.x = T)
  
  rf_minput<-merge(rf_minput,id_latlong,by='ID',all.x = T)
  
  rf_minput<-merge(rf_minput,indices[,3:8],by='id_month',all.x = T)
  

  rf_ninput<-rf_minput
  rf_ninput<-rf_ninput[complete.cases(rf_ninput),]
  rf_ninput <- data.table(rf_ninput)
  #write.table(rf_ninput,'C://Users//61416//Desktop//tidyfile//meanair_07.csv',sep = ',')
  
  rf_ninput <- rf_ninput[,-c(1)]
  
  set.seed(100)
  at_ninput <- CreateSpacetimeFolds(rf_ninput,spacevar = "ID",timevar='month',k=10)
  #at_ninput <- CreateSpacetimeFolds(rf_ninput,timevar='month',k=10)
  #at_ninput <- CreateSpacetimeFolds(rf_ninput,spacevar = "ID",k=10)
  #set.seed(40)
  #tempvarr <- rf_ninput[,-c(1,2,5)]
  model_LLTO <- train(rf_ninput[,-c(1,2,5)],rf_ninput$at,
                     method="rf",tuneLength=1, importance=TRUE,
                     trControl=trainControl(method="cv",index = at_ninput$index))
  #a <- model_LLTO$results
  ##xgbDARTpath <- paste0('D:/SCI_Paper/Paper2/method_selection/monthly_results_LTO/',i,'_xgbDART.csv')
  ##write.table(a,xgbDARTpath)
  
  
  #plot(varImp(model_LLTO))
  ##
  ##
  #ffsmodel <- ffs(rf_ninput[,-c(1,2,5)],rf_ninput$avt,method="cubist",
  #                tuneLength=1,importance=TRUE,
  #                trControl=trainControl(method="cv",
  #                index = at_ninput$index))
  
  #ffsmodel
  ##plot(varImp(ffsmodel))
  ##
  ynumeric <- as.numeric(i)
  start <- 100*ynumeric+1
  end   <- 100*ynumeric+12
  Ymonth <-c(start:end)
  Ymonth <- sprintf("%04d", Ymonth)
  #
  for (emonth in Ymonth){
    NDVIpath <- paste0("F://TP_rasterdatasets//Resample_indices//",emonth,"_NDVI.tif")
    EVIpath  <- paste0("F://TP_rasterdatasets//Resample_indices//",emonth,"_EVI.tif")
    LSWIpath <- paste0("F://TP_rasterdatasets//Resample_indices//",emonth,"_LSWI.tif")
    SAVIpath <- paste0("F://TP_rasterdatasets//Resample_indices//",emonth,"_SAVI.tif")
    NDSIpath <- paste0("F://TP_rasterdatasets//Resample_indices//",emonth,"_NDSI.tif")
    srpath   <- paste0("F://TP_rasterdatasets//Reference_MSR//",emonth,"_MSR.tif")
    LSTminpath  <- paste0("F://TP_rasterdatasets//Resample_LST/20",i,"_Monthly_Min_LST//",emonth,"_Tmin.tif")
    #print(LSTminpath)
    LSTmaxpath  <- paste0("F://TP_rasterdatasets//Resample_LST/20",i,"_Monthly_Max_LST//",emonth,"_Tmax.tif")
    NDVI <- raster(NDVIpath)
    EVI  <- raster(EVIpath)
    LSWI <- raster(LSWIpath)
    SAVI <- raster(SAVIpath)
    NDSI <- raster(NDSIpath)
    sr   <- raster(srpath)
    LSTnight  <- raster(LSTminpath)
    LSTday  <- raster(LSTmaxpath)
    
    ALT <- raster("F:/TP_rasterdatasets/Resample_DEM/TP_DEM.tif")
    LAT <- raster("F:/TP_rasterdatasets/Resample_DEM/TP_Latc.tif")
    LON <- raster("F:/TP_rasterdatasets/Resample_DEM/TP_Lonc.tif")
    TCI <- raster("F:/TP_rasterdatasets/Resample_DEM/TP_TCI.tif")
    
    predictors_sp <- stack(NDVI,EVI,LSWI,SAVI,NDSI,sr,LSTnight,LSTday,ALT,LAT,LON)
    names(predictors_sp) <- c('NDVI','EVI','LSWI','SAVI','NDSI','sr','LSTnight','LSTday','ALT','LAT','LON')
    #predictors_sp
    #predictors_sp <- stack(EVI,sr,LSTnight,ALT,LAT)
    #names(predictors_sp) <- c('EVI','sr','LSTnight','ALT','LAT')
    
    prediction <- predict(predictors_sp,model_LLO)
    
    
    outputpath <- paste0("C:/Users/61416/Desktop/tidyfile/predictors/Tmin/",emonth,"_tavg.tif")
    
    writeRaster(prediction$layer,outputpath,overwrite=TRUE)
  #   
  #  
  }
  
  
}

stopCluster(cl)

rf_predict<-predict(model_LLO,predictors_sp)


spplot(prediction)


library(raster)
library(rasterVis)
listFich <- dir(pattern='\\.tif')
stackSIS <- stack(listFich)
#stackSIS <- stackSIS * 24 ##from irradiance (W/m2) to irradiation Wh/m2

idx <- seq(as.Date('2007-01-01'), as.Date('2007-12-01'), 'month')
idx
SISmm <- setZ(stackSIS, idx)
names(SISmm) <- month.abb

levelplot(SISmm,par.settings = BuRdTheme)


listFich <- dir(pattern='\\.tif')
stackSIS <- stack(listFich)
a <- levelplot(stackSIS, xlab="", ylab="",margin=FALSE,colorkey=list(space="bottom"),layout = c(1, 3, 1))
listFich <- dir(pattern='\\.tif')
stackSIS <- stack(listFich)
a+levelplot(stackSIS, xlab="", ylab="",margin=FALSE,colorkey=list(space="bottom"),layout = c(2, 3, 1))


library(gridExtra)

mapTheme <- rasterTheme(region=rev(brewer.pal(10,'Spectral')))

listFich_MT <- dir('D:/SCI_Paper/Paper2/RasterData/MT/',pattern='\\.tif')
filepathlist_MT <- paste0('D:/SCI_Paper/Paper2/RasterData/MT/',listFich_MT)
stackSIS_MT <- stack(filepathlist_MT)
a <- levelplot(stackSIS_MT, xlab="", ylab="",scales=list(draw=TRUE),at = do.breaks(c(-0.4,0.4),100),margin=F,par.settings=mapTheme,colorkey=list(space="bottom"),layout = c(1, 3),main='MTRS Decadal Warm Trending')


listFich_NCEP <- dir('D:/SCI_Paper/Paper2/RasterData/NCEP/',pattern='\\.tif')
filepathlist_NCEP <- paste0('D:/SCI_Paper/Paper2/RasterData/NCEP/',listFich_NCEP)
stackSIS_NCEP <- stack(filepathlist_NCEP)
b <- levelplot(stackSIS_NCEP, xlab="", ylab="",scales=list(draw=TRUE),at = do.breaks(c(-0.3,0.3),100),margin=F,par.settings=mapTheme,colorkey=list(space="bottom"),layout = c(1, 3),main='NECP Decadal Warm Trending')


listFich_GLDAS <- dir('D:/SCI_Paper/Paper2/RasterData/GLDAS/',pattern='\\.tif')
filepathlist_GLDAS <- paste0('D:/SCI_Paper/Paper2/RasterData/GLDAS/',listFich_GLDAS)
stackSIS_GLDAS <- stack(filepathlist_GLDAS)



c <- levelplot(stackSIS_GLDAS[1], xlab="", ylab="",scales=latscales,at = do.breaks(c(-0.3,0.3),100),margin=F,par.settings=mapTheme,colorkey=list(space="bottom"),layout = c(1, 3),main='GLDAS Decadal Warm Trending')

c

mm <- grid.arrange(a,b,c,nrow=1,ncol=3)
mm

x <- 1:20 
y1 <- rnorm(20)*1e3 
y2 <- rnorm(20)*1e3 
xyplot(y1 + y2 ~ x, type='l', 
       scales = list(y = list( 
         at = seq(-300,400,200), 
         limits = c(-500,500))) 
) 



a <- levelplot(raster(filepathlist_MT[1]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=list(draw='False',space="bottom"),main='MTRS Decadal Warm Trending')


rescales <- list(y=list(at = seq(25,40,5),limits = c(25,41)),x=list(at = seq(75,105,6),limits = c(73,105)))
a1 <- levelplot(raster(filepathlist_MT[1]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=FALSE,main='Machine learning-based Decadal Warm Trending')
a2 <- levelplot(raster(filepathlist_MT[2]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=FALSE)
a3 <- levelplot(raster(filepathlist_MT[3]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=list(draw='TRUE',space="bottom"))

b1 <- levelplot(raster(filepathlist_NCEP[1]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=FALSE,main='NCEP Decadal Warm Trending')
b2 <- levelplot(raster(filepathlist_NCEP[2]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=FALSE)
b3 <- levelplot(raster(filepathlist_NCEP[3]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=list(draw='TRUE',space="bottom"))

c1 <- levelplot(raster(filepathlist_GLDAS[1]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=FALSE,main='GLDAS Decadal Warm Trending')
c2 <- levelplot(raster(filepathlist_GLDAS[2]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=FALSE)
c3 <- levelplot(raster(filepathlist_GLDAS[3]), xlab="", ylab="",scales=rescales,margin=F,par.settings=mapTheme,colorkey=list(draw='TRUE',space="bottom"))


nn <- grid.arrange(a1,b1,c1,a2,b2,c2,a3,b3,c3,nrow=3,ncol=3)
library(grid)
library(lattice)
grid.rect(gp=gpar(fill=NA))
nn

a1



listFich <- dir(pattern='\\.tif')

glads_raster <- raster(listFich[1])
mtrs_raster <- raster(listFich[2])
ncep_raster <- raster(listFich[3])
mtrs_raster_resized <- resample(mtrs_raster,glads_raster)
ncep_raster_resized  <- resample(ncep_raster,glads_raster)

stackSIS <- stack(mtrs_raster_resized,glads_raster,ncep_raster_resized)


mapTheme <- rasterTheme(region=rev(brewer.pal(10,'Spectral')))

rescales <- list(y=list(at = seq(25,40,5),limits = c(25,41)),x=list(at = seq(75,105,6),limits = c(73,105)))
listFich_MT <- dir('D:/SCI_Paper/Paper2/RasterData/MT/',pattern='\\.tif')
filepathlist_MT <- paste0('D:/SCI_Paper/Paper2/RasterData/MT/',listFich_MT)

zero <- '#ffffff'
reds <- brewer.pal('Reds',n=7)
blues <- rev(brewer.pal('Blues',n=3))
MTtheme = rasterTheme(region=c(blues,zero,reds))

a <- levelplot(raster(filepathlist_MT[1]), xlab="", ylab="",scales=rescales,at = do.breaks(c(-0.04,0.08),12),margin=FALSE,par.settings=MTtheme,colorkey=list(space="bottom"),main='Machine Learning-based monthly warming trending')
a


zero <- '#ffffff'
reds <- brewer.pal('Reds',n=5)
blues <- rev(brewer.pal('Blues',n=5))
NCEPtheme = rasterTheme(region=c(blues,reds))
listFich_NCEP <- dir('D:/SCI_Paper/Paper2/RasterData/NCEP/',pattern='\\.tif')
filepathlist_NCEP <- paste0('D:/SCI_Paper/Paper2/RasterData/NCEP/',listFich_NCEP)
c <- levelplot(raster(filepathlist_NCEP[1]), xlab="", ylab="",scales=rescales,at = do.breaks(c(-0.06,0.06),12),margin=FALSE,par.settings=NCEPtheme,colorkey=list(space="bottom"),main='NCEP monthly warming trending')
c

zero <- '#ffffff'
reds <- brewer.pal('Reds',n=9)
blues <- rev(brewer.pal('Blues',n=3))
GLDAStheme = rasterTheme(region=c(blues,zero,reds))
listFich_GLDAS <- dir('D:/SCI_Paper/Paper2/RasterData/GLDAS/',pattern='\\.tif')
filepathlist_GLDAS <- paste0('D:/SCI_Paper/Paper2/RasterData/GLDAS/',listFich_GLDAS)
b <- levelplot(raster(filepathlist_GLDAS[1]), xlab="", ylab="",scales=rescales,at = do.breaks(c(-0.08,0.2),15),margin=FALSE,par.settings=GLDAStheme,colorkey=list(space="bottom"),main='GLDAS monthly warming trending')
b

library(gridExtra)
lay <- rbind(c(1,1),
             c(2,3))
nn <- grid.arrange(a,b,c,heights = cbind(c(2,2,1,1)),widths=rbind(c(2,2,1,1)),layout_matrix = rbind(c(1, 1),c(2,3)))




library(raster)



btp <- shapefile('F:/TP_SP_Map/TP_BJ/boundary.shp')
plot(btp,axes=TRUE,main='Monthly warming trending in weather stations')

ptp <- shapefile("C:/Users/61416/Desktop/tidyfile/MK_test_cooling.shp")
bb <- plot(ptp,add=TRUE,col='blue')
wtp <- shapefile("C:/Users/61416/Desktop/tidyfile/MK_test_warming.shp")
cc <- plot(wtp,add=TRUE,col='red')

legend('topright', bb,lty=1, col=c('blue'), bty='n', cex=.75)



