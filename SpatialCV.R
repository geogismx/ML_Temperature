library(lubridate)
library(data.table)
library(randomForest)
library(caret)
library(stringr)

maxtemp<-fread('C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/tidy_maxtemp/maxtempcol_2007.csv')

maxtemp<-maxtemp[,-c(2,3)]

maxtemp<-reshape2::melt(maxtemp,id.vars='ID')

colnames(maxtemp)[1:3]<-c('ID','date','at')


#splitout <- data.frame(str_split_fixed(maxtemp$date,"X",2)[,2])
#colnames(splitout) <- c("Date")
#maxtemp <- cbind(maxtemp,splitout)
#maxtemp<-maxtemp[,c(1,3,4)]

maxtemp$date<-as.Date(maxtemp$date,"%Y/%m/%d")

maxtemp$date <- substr(maxtemp$date,6,10)

#maxtemp<- maxtemp[which(maxtemp$date=="01-01"|maxtemp$date=="01-02"|maxtemp$date=="01-03"),]


maxtemp$id_date<-paste0(maxtemp$ID,'_',maxtemp$date)



########################solar radiation

sr<-fread('C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/tidy_RS/RS-2007.csv')

sr<-sr[,-c(1,3,4)]

sr<-reshape2::melt(sr,id.vars='ID')

#unique(sr$variable)

colnames(sr)[2:3]<-c('date','sr')

sr <- data.frame(sr)

sr$date<-substr(sr$date,6,10)

#sr<- sr[which(sr$date=="01-01"|sr$date=="01-02"|sr$date=="01-03"),]


sr$id_date<-paste0(sr$ID,'_',sr$date)


###########################LST

lst <- fread("C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/tidy_LST_Day/LST-ST-2007.csv")

lst <- lst[,-c(1,3,4)]

lst<-reshape2::melt(lst,id.vars='ID')

colnames(lst)[2:3]<-c('date','lst')

#lst$date<-as.Date(lst$date,"%Y/%m/%d")

lst <- data.frame(lst)

lst$date<-substr(lst$date,6,10)

#lst<- lst[which(lst$date=="01-01"|lst$date=="01-02"|lst$date=="01-03"),]

lst$id_date<-paste0(lst$ID,'_',lst$date)


###################################################ele 

ele<-fread('D:/SCI_Paper/Paper2/Data/tp_airtemp.csv')

id_latlong<-ele[,c(1:3)]

ele<-ele[,c(1,4)]


##################################################indices

indices<-fread("C:/Users/61416/Desktop/tidyfile/TP_Airtemp_Trainingdata/tidy_indices/Indices-2007.csv")

indices<-indices[,-c(1,3,4)]

#indices$Date<-as.Date(indices$Date,"%Y/%m/%d")

indices$Date<-substr(indices$Date,6,10)

#indices$indices<-substr(indices$variable,7,7)
indices <- data.frame(indices)

#indices<- indices[which(indices$Date=="01-01"|indices$Date=="01-02"|indices$Date=="01-03"),]
#indices<-indices[,-2]

indices$id_date<-paste0(indices$ID,'_',indices$Date)

rf_input<-maxtemp

rf_input<-merge(rf_input,ele,by='ID',all.x = T)

rf_input<-merge(rf_input,sr[,3:4],by='id_date',all.x = T)

rf_input<-merge(rf_input,lst[,3:4],by='id_date',all.x = T)

rf_input<-merge(rf_input,id_latlong,by='ID',all.x = T)

rf_input<-merge(rf_input,indices[,3:8],by='id_date',all.x = T)


tp_pw <- fread("D:/SCI_Paper/Paper2/Data/climate_zone_partition/tp_pw_points.csv")
tp_pyh <- fread("D:/SCI_Paper/Paper2/Data/climate_zone_partition/tp_pyh_points.csv")
tp_zyh <- fread("D:/SCI_Paper/Paper2/Data/climate_zone_partition/tp_zyh_points.csv")

tp_pw<-tp_pw[,c(1,8)]
tp_pyh <- tp_pyh[,c(1,8)]
tp_zyh <- tp_zyh[,c(1,8)]


tp_cz <- rbind(tp_pw,tp_pyh)
tp_cz <- rbind(tp_cz,tp_zyh)

tp_cz$QU1 <- as.factor(tp_cz$QU1)

rf_input<-merge(rf_input,tp_cz,by='ID',all.x = T)




#rf_input$date<-yday(rf_input$date)

#rf_input$ddl <- (24/pi)*acos(tan(pi*rf_input$LAT/180)*tan((23.45*pi/180)*sin(2*pi*(284+rf_input$date)/365)))

####################################change table columns postion

rf_input_07<-rf_input[,-c(-1)]

####################################rf_input contains missing data

####################################create a complete subset from rf_input

rf_input_07<-rf_input_07[complete.cases(rf_input_07),]

rf_input_07<-rf_input_07[,-c(1)]
#setwd("E:/TPDEM/airtemp/rf_results")

#fwrite(rf_input,'rf_input_2009.csv')








library(CAST)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)
#rf_input

rf_input_03<-rf_input_03[,-c(2,3)]
rf_input_04<-rf_input_04[,-c(2,3)]
rf_input_05<-rf_input_05[,-c(2,3)]
rf_input_06<-rf_input_06[,-c(2,3)]
rf_input_07<-rf_input_07[,-c(2,3)]
rf_input_08<-rf_input_08[,-c(2,3)]
rf_input_09<-rf_input_09[,-c(2,3)]
rf_input_10<-rf_input_10[,-c(2,3)]
rf_input_11<-rf_input_11[,-c(2,3)]
rf_input_12<-rf_input_12[,-c(2,3)]
rf_input_13<-rf_input_13[,-c(2,3)]

####################################rf_input contains missing data

####################################create a complete subset from rf_input

#rf_input<-rf_input[complete.cases(rf_input),]
#rf_model<-randomForest(x=traindata[,1:9],y=traindata$at,mtry = 4,ntree = 500,importance = T)

set.seed(40)
atindices_03 <- CreateSpacetimeFolds(rf_input_03,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_03 <- train(rf_input_03[,3:13],rf_input_03$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_03$index))

atindices_04 <- CreateSpacetimeFolds(rf_input_04,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_04 <- train(rf_input_04[,3:13],rf_input_04$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_04$index))


atindices_05 <- CreateSpacetimeFolds(rf_input_05,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_05 <- train(rf_input_05[,3:13],rf_input_05$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_05$index))


atindices_06 <- CreateSpacetimeFolds(rf_input_06,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_06 <- train(rf_input_06[,3:13],rf_input_06$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_06$index))


atindices_07 <- CreateSpacetimeFolds(rf_input_07,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_07 <- train(rf_input_07[,3:13],rf_input_07$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = rf_input_07$index))










atindices_08 <- CreateSpacetimeFolds(rf_input_08,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_08 <- train(rf_input_08[,3:13],rf_input_08$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_08$index))


atindices_10 <- CreateSpacetimeFolds(rf_input_10,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_10 <- train(rf_input_10[,3:13],rf_input_10$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_10$index))

atindices_11 <- CreateSpacetimeFolds(rf_input_11,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_11 <- train(rf_input_11[,3:13],rf_input_11$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_11$index))

atindices_12 <- CreateSpacetimeFolds(rf_input_12,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_12 <- train(rf_input_12[,3:13],rf_input_12$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_12$index))


atindices_13 <- CreateSpacetimeFolds(rf_input_13,spacevar = "ID",
                                     k=10)
#set.seed(40)
model_LLO_13 <- train(rf_input_13[,3:13],rf_input_13$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = atindices_13$index))



# model_LLO_03
# 
# model_LLO_04
# 
# model_LLO_05
# 
# model_LLO_06

atindices_07 <- CreateSpacetimeFolds(rf_input_07,spacevar = "ID",k=10)
#set.seed(40)
model_LLO_07 <- train(rf_input_07[,3:13],rf_input_07$at,
                      method="rf",tuneLength=1, importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = rf_input_07$index))

model_LLO_07
# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

library(caretEnsemble)

atindices_07 <- CreateSpacetimeFolds(rf_input_07,spacevar = "ID",k=5)
set.seed(100)

trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
trainControl=trainControl(method="cv",index = atindices_07$index,savePredictions=TRUE,classProbs=TRUE)


algorithmList <- c('rf','cubist', 'xgbDART', 'svmRadial')

rf_input_07 <- rf_input_07[,2:13]

models <- caretList(at ~ ., data=rf_input_07, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


# model_LLO_08
# 
# model_LLO_10
# 
# model_LLO_11
# 
# model_LLO_12
# 
# model_LLO_13


# plot(varImp(model_LLO_03))
# plot(varImp(model_LLO_04))
# plot(varImp(model_LLO_05))
# plot(varImp(model_LLO_06))
# plot(varImp(model_LLO_07))
# plot(varImp(model_LLO_10))
# plot(varImp(model_LLO_11))
# plot(varImp(model_LLO_12))
# plot(varImp(model_LLO_13))


# set.seed(40)
# 
# atindices_03 <- CreateSpacetimeFolds(rf_input_03,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_03 <- ffs(rf_input_03[,3:13],rf_input_03$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_03$index))
# 
# 
# 
# atindices_04 <- CreateSpacetimeFolds(rf_input_04,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_04 <- ffs(rf_input_04[,3:13],rf_input_04$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_04$index))
# 
# 
# 
# atindices_05 <- CreateSpacetimeFolds(rf_input_05,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_05 <- ffs(rf_input_05[,3:13],rf_input_05$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_05$index))
# 
# 
# 
# atindices_06 <- CreateSpacetimeFolds(rf_input_06,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_06 <- ffs(rf_input_06[,3:13],rf_input_06$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_06$index))
# 
# 
# 
# atindices_07 <- CreateSpacetimeFolds(rf_input_07,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_07 <- ffs(rf_input_07[,3:13],rf_input_07$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_07$index))
# 
# 
# 
# atindices_08 <- CreateSpacetimeFolds(rf_input_08,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_08 <- ffs(rf_input_08[,3:13],rf_input_08$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_08$index))
# 
# 
# 
# atindices_09 <- CreateSpacetimeFolds(rf_input_09,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_09 <- ffs(rf_input_09[,3:13],rf_input_09$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_09$index))
# 
# 
# 
# atindices_10 <- CreateSpacetimeFolds(rf_input_10,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_10 <- ffs(rf_input_10[,3:13],rf_input_10$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_10$index))
# 
# 
# atindices_11 <- CreateSpacetimeFolds(rf_input_11,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_11 <- ffs(rf_input_11[,3:13],rf_input_11$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_11$index))
# 
# 
# 
# atindices_12 <- CreateSpacetimeFolds(rf_input_12,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_12 <- ffs(rf_input_12[,3:13],rf_input_12$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_12$index))
# 
# 
# 
# atindices_13 <- CreateSpacetimeFolds(rf_input_13,spacevar = "ID",
#                                      k=10)
# ffsmodel_LLO_13 <- ffs(rf_input_13[,3:13],rf_input_13$at,metric="Rsquared",
#                        method="rf",tuneLength=1, verbose=FALSE,
#                        trControl=trainControl(method="cv",
#                                               index = atindices_13$index))


# ffsmodel_LLO_03
# ffsmodel_LLO_04
# ffsmodel_LLO_05
# ffsmodel_LLO_06
# ffsmodel_LLO_07
# ffsmodel_LLO_08
# ffsmodel_LLO_09
# ffsmodel_LLO_10
# ffsmodel_LLO_11
# ffsmodel_LLO_12
# ffsmodel_LLO_13


stopCluster(cl)