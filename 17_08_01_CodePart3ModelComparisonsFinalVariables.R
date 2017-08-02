
#Mari K Reeves
#7/25/17
#Kenai Road Model Code Part 2 --- Model Comparison
#
# Remove everything in workspace
rm(list = ls())
graphics.off()


# Read in Base Packages ---------------------------------------------------

pckg <- c('RColorBrewer','MASS','stringr',
          'glmnet', 'colorRamps', 'plyr', 'tidyr', 'R.utils',
          'stringr', 'forestFloor', 'car',
          'randomForestSRC', 'ggRandomForests',
          'scatterplot3d',
          'dismo','gbm', 'boot', 'spatialEco', 'pscl', 
          'R.utils','stringr','raster','rgdal','rgeos',
           'curl', 'RCurl',
          'lme4', 'dplyr', 'survival','pROC',
          'boot', 'vegan', 'zoo', 'reshape2',
          'pscl', 'randomForestSRC', 'ggplot2','kernlab',
          'coda', 'languageR', 'lmerTest') 


# READING IN PACKAGES
for(i in 1:length(pckg)){
  if ((!pckg[i] %in% installed.packages())==T) {
    install.packages(pckg[i], repos="http://cran.us.r-project.org", 
                     dependencies = T)
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }else{
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }
}

#Run and compare models in caret: https://www.jstatsoft.org/article/view/v028i05
#It will ask you to restart R. Just say yes. Doesn't always go perfectly. Watch this step.
install.packages("caret", dependencies = c("Depends", "Suggests"))
#then it will stop, so run this. I am doing it separately, so that it can suggest package dependencies when needed
library(caret)



#Set Up Workspace
space<-"E:/1New/RoadsAnalysis/"
AnalysisDir <- paste0(space, 'KenaiInputForEdgeEffect/')
ShapeDir <- paste0(AnalysisDir, "GIS/FinalModels")
OutputDir <- paste0(AnalysisDir, 'Analysis_Output/')
if (file.exists(OutputDir)==F){
  dir.create(OutputDir)
}
setwd(paste0(AnalysisDir, 'Analysis_Output/'))


#Read In BaseData created in prior code...This is on GitHub
BaseData <- read.csv(paste0(OutputDir, "17_07_25_FinalVariables4Model.csv"))
#getwd()
#Make a copy of BaseData
allData<-BaseData

allData<-allData[complete.cases(allData),] # Removes rows with NAs, just in case
names(allData)

#~~~~~~~~~~~~~~~~Generate Some Alternate Datasets for Predictors
# [1] "X"                   "Site"                "Date"               
#[4] "Location.x"          "Ca"                  "Cr"                 
#[7] "Cu"                  "Fe"                  "Mg"                 
#[10] "Mn"                  "Na"                  "Ni"                 
#[13] "Pb"                  "V"                   "Zn"                 
#[16] "date"                "year"                "ID"                 
#[19] "ID4RanEf"            "Location.y"          "StreetName"         
#[22] "Zone"                "RoadType"            "RoadSurface"        
#[25] "LandCoverCode"       "PercTree"            "PercPaved"          
#[28] "LandCoverName"       "LandCoverCategory"   "LandCoverDefinition"
#[31] "LandUse"             "LandOwnership"       "LandUseIntensity"   
#[34] "LandUseCat"          "Paved"               "good"               
#[37] "bad"                 "good2bad"          

#Reduce the raw data to relevant variables and assign better names
BaseData<-BaseData[,c("Location.x","year","RoadType","PercTree","PercPaved","LandCoverCategory","Paved","bad")]
names(BaseData)<-c("DistanceToRoad","Year","RoadTraffic","ForestCover","ImperviousSurface","LandCover","PavedRoad","Metals")


#create lists of variables for factor and numeric assignment
VarListFactors<-c("RoadTraffic","Year","PavedRoad","LandCover")
VarListNumeric<-c("DistanceToRoad","ForestCover","ImperviousSurface","Metals")

#assign variables to factor or numeric
BaseData[,VarListFactors] = lapply(BaseData[,VarListFactors], 
                                 function(x) as.factor(as.character(x)))
BaseData[,VarListNumeric] = lapply(BaseData[,VarListNumeric], 
                                   function(x) as.numeric(as.character(x)))
#~~~~~~~~~~~Take a look at predictor structure
str(BaseData)
pairs(BaseData)
plot(BaseData$ImperviousSurface~BaseData$Paved)
plot(BaseData$Paved~BaseData$LandCover)
plot(allData$PercPaved~allData$LandCoverName)
hist(BaseData$ImperviousSurface)
plot(BaseData$ForestCover)
hist(BaseData$ForestCover)
plot(BaseData$ImperviousSurface)

plot(BaseData$Paved~BaseData$ImperviousSurface)

plot(BaseData$ForestCover~BaseData$Paved)

plot(BaseData$ForestCover~BaseData$ImperviousSurface)

#deal with this nasty correlation between forest cover and impervious surface
#by transforming Impervious Surface into Unpaved Surface
#To do this subtract from 100
BaseData$UnpavedSurface<-100 - BaseData$ImperviousSurface
#Now add to the Percent Forest Cover data to give a high number as natural forested
#a mid number as natural non-forested or forested but impervious surface
#A low number is impervious and not forested - predicted high metal concentration and 
#transport
BaseData$UnpavedForest<-BaseData$UnpavedSurface+BaseData$ForestCover
hist(BaseData$UnpavedForest)
plot(BaseData$UnpavedForest~BaseData$ImperviousSurface)
plot(BaseData$UnpavedForest~BaseData$ForestCover)
hist(sqrt(BaseData$UnpavedForest))
hist(log(BaseData$UnpavedForest))


#We could use the createDataPartion function in caret to split the data in stratefied random way

set.seed(333)
splitData <- createDataPartition(BaseData$Metals, p = 0.75, list = FALSE, times = 1)
randomtrain<-BaseData[splitData,]
randomtest<-BaseData[-splitData,]
names(randomtrain)
#because we replicated these samples in time, we can split the data into training and 
#testing data sets by year.Training will be 2010 (which has more transect points at 
#fewer sites) and 2011 (which has fewer points at more sites)
#I'm making them separately first, so I can combine them at will later
train2010<-BaseData[BaseData$Year == "2010",]
train2011 <- BaseData[BaseData$Year == "2011",] 
#We'll see how good we are at predicting 2012!
test2012 <- BaseData[BaseData$Year == "2012",] 

train1011<-rbind(train2010,train2011)
names(train1011)
#make a list of these datasets in case I can do things in batch
datalist<-c("randomtrain","train2010","train2011","train1011","randomtest","test2012")
trainlist<-c("randomtrain","train2010","train2011","train1011")
testlist<-c("randomtest","test2012")


#now, drop a few variables I don't want  in the analysis 

datalist<-c("randomtrain","train2010","train2011","train1011","randomtest","test2012")
keeps <- c("DistanceToRoad","RoadTraffic","ImperviousSurface","LandCover","PavedRoad","Metals")


for (dat in datalist) {
  jnk<-get(dat)[,keeps]
  assign(paste0(dat), jnk)
}


#simple split with Site as a group: http://topepo.github.io/caret/data-splitting.html



#This specifies 10 fold cross validation with 10 repeats default summary for regression
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 15, returnData = TRUE, savePredictions = "final",
                           verboseIter = FALSE, returnResamp = "final" )

#check for variables with near zero variance in each data set
lapply(datalist,nearZeroVar)

#check for correlated predictors
ncol(train1011)
traincor<-cor(train1011[,c("DistanceToRoad","UnpavedForest")])
highcor <- findCorrelation(traincor, 0.70)
#there were none. Not surprising with three numeric predictors, but useful code
#and good practice with intent to run linear models.
#and as a side note, this did not pick up the nasty zero inflated impervious surface
#forest cover correlation dealt with earlier

#The train function can be used to select values of model tuning parameters and/or
#estimate model performance using resampling.
#x: a matrix or data frame of predictors. 
#y: a numeric or factor vector of outcomes.
#method: a character string specifying the type of model to be used.
#metric: a character string with values of "Accuracy", "Kappa", "RMSE" or "Rsquared".

#Make a method list to use in a lapply statement for the train functions
models<-c("rf","gbm","earth","glmStepAIC","bagEarthGCV", "glmnet")


## Random Forests in caret 
rfgrid<-expand.grid(mtry = 1:4)
rfit1 <- train(Metals ~ ., data = train1011, method = "rf", trControl = fitControl, tuneGrid = rfgrid, importance = T)
print(rfit1)
plot(rfit1)
summary(rfit1)
rfImp1 <- varImp(rfit1, scale = T, useModel = FALSE)  #If usemodel = T plots each individual variable
plot(rfImp1)
rfImp1 <- varImp(rfit1, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(rfImp1)

rfTest <- predict(rfit1, test2012)
plot(rfTest~test2012$Metals, xlim = c(-2, 5), ylim = c(-2, 2))
abline(0, 1)

##Drop the outlier and re-run - - this made no difference, so we're not going to worry 
#about the outlier in the test dataset. Only in the training dataset. 
test2012no_out<-test2012[-24,]
test2012no_out$Metals
rfTest_no_out <- predict(rfit1, test2012no_out)

plot(rfTest_no_out~test2012no_out$Metals, xlim = c(-2, 5), ylim = c(-2, 2))
abline(0, 1)

#Run a normal random forest model to get all the outputs in the package
?randomForest
rf<-randomForest(Metals ~ ., data = train1011, mtry = 2, strata = DistanceToRoad, importance = T)
summary(rf)
plot(rf$y~rf$predicted)
abline(0,1)
rf$importance

##Boosted Regression in caret 
gbmGrid <- expand.grid(interaction.depth = (1:4), n.trees = (1:30)*30, shrinkage = 0.01,
                       n.minobsinnode = 10)
gbmFit1<-train(Metals ~ ., data = train1011, method = "gbm", tuneGrid = gbmGrid, trControl = fitControl)
gbmFit1
plot(gbmFit1)
summary(gbmFit1$results)
gbmImp1 <- varImp(gbmFit1, scale = TRUE, useModel = TRUE)
plot(gbmImp1)

gbmTest1 <- predict(gbmFit1, test2012)
plot(gbmTest1~test2012$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0,1)

#Partial Least Squares
plsgrid <- data.frame(ncomp = 1:5)
plsFit1<-train(Metals ~ ., data = train1011, method = "pls", trControl = fitControl, tuneGrid = plsgrid)
plsFit1
summary(plsFit1)
plot(plsFit1)
plsImp1 <- varImp(plsFit1, scale = TRUE, useModel = T)
plot(plsImp1)

plsTest1<- predict(plsFit1, test2012)
plot(plsTest1~test2012$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0, 1)



#Bagged Earth Regression Spline with CV pruning
earthFit1<-train(Metals ~ ., data = train1011, method = "bagEarthGCV", trControl = fitControl)
earthFit1
summary(earthFit1)
earthImp1 <- varImp(earthFit1, scale = TRUE, useModel = TRUE)
plot(earthImp1)
earthTest1<- predict(earthFit1, test2012)
plot(earthTest1~test2012$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0, 1)

#generalized linear regression stepwise feature selection 
glmstepFit1<-train(Metals ~ ., data = train1011, method = "glmStepAIC", trControl = fitControl)
glmstepFit1
summary(glmstepFit1)
glmstepImp1<-varImp(glmstepFit1, scale = TRUE, useModel = TRUE)
plot(glmstepImp1)
GLMTest<- predict(glmstepFit1, test2012)
plot(GLMTest~test2012$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0, 1)


#Elastic Net Regression in caret 
eFit1 <- train(Metals ~ ., data = train1011, method = "glmnet", trControl = fitControl)
print(eFit1)
plot(eFit1)
summary(eFit1)

eImp <- varImp(eFit1, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(eImp)

eTest <- predict(eFit1, test2012)
plot(eTest~test2012$Metals, xlim = c(-2, 5), ylim = c(-2, 2))
abline(0, 1)


#~~~~~~~~~~~~~~~~~~~~~~~~~Retrain Models with Randomly Subset Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Random Forests in caret 
rfit2 <- train(Metals ~ ., data = randomtrain, method = "rf", trControl = fitControl, tuneGrid = rfgrid,  importance = T)
print(rfit2)
plot(rfit2)
summary(rfit2)
rfImp2 <- varImp(rfit2, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(rfImp2)

rfTest <- predict(rfit2, randomtest)
plot(rfTest~randomtest$Metals, xlim = c(-2, 5), ylim = c(-2, 2))
abline(0, 1)


##Boosted Regression in caret
gbmGrid <- expand.grid(interaction.depth = (1:4), n.trees = (1:30)*30, shrinkage = 0.01,
                       n.minobsinnode = 20)
gbmFit2<-train(Metals ~ ., data = randomtrain, method = "gbm", tuneGrid = gbmGrid, trControl = fitControl)
gbmFit2
plot(gbmFit2)
summary(gbmFit2$results)
gbmImp2 <- varImp(gbmFit2, scale = TRUE, useModel = TRUE)
plot(gbmImp2)

gbmTest2 <- predict(gbmFit2, randomtest)
plot(gbmTest2~randomtest$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0,1)

#Partial Least Squares
plsFit2<-train(Metals ~ ., data = randomtrain, method = "pls", trControl = fitControl, tuneGrid = plsgrid)
plsFit2
summary(plsFit2)
plsImp1 <- varImp(plsFit2, scale = TRUE, useModel = T)
plot(plsImp1)

plsTest2<- predict(plsFit2, randomtest)
plot(plsTest2~randomtest$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0, 1)



#Bagged Earth models with CV pruning
earthFit2<-train(Metals ~ ., data = randomtrain, method = "bagEarthGCV", trControl = fitControl)
earthFit2
summary(earthFit2)

earthImp2 <- varImp(earthFit2, scale = TRUE, useModel = TRUE)
plot(earthImp2)
earthTest2<- predict(earthFit2, randomtest)
plot(earthTest2~randomtest$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0, 1)

#generalized linear regression stepwise feature selection
glmstepFit2<-train(Metals ~ ., data = randomtrain, method = "glmStepAIC", trControl = fitControl)
glmstepFit2
summary(glmstepFit2)

glmstepImp2<-varImp(glmstepFit2, scale = TRUE, useModel = TRUE)
plot(glmstepImp2)
GLMTest<- predict(glmstepFit2, randomtest)
plot(GLMTest~randomtest$Metals, xlim = c(-2, 4), ylim = c(-2, 4))
abline(0, 1)


#Elastic Net Regression in caret 
eFit2 <- train(Metals ~ ., data = randomtrain, method = "glmnet", trControl = fitControl)
print(eFit2)
plot(eFit2)
summary(eFit2)

eImp2 <- varImp(eFit2, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(eImp2)

eTest2 <- predict(eFit2, randomtest)
plot(eTest2~randomtest$Metals, xlim = c(-2, 5), ylim = c(-2, 2))
abline(0, 1)




## Model Selection ##
#Make a model list to use in a lapply statement for the train functions
models<-c("rf","gbm","pls","glmStepAIC","bagEarthGCV", "glmnet")

resamps <- resamples(list(RFYear = rfit1, RFRandom = rfit2, GBMYear = gbmFit1,GBMRandom = gbmFit2, 
                          PLSYear = plsFit1, PLSRandom = plsFit2 , GLMAICYear = glmstepFit1, GLMAICRandom = glmstepFit2, 
                          BagEarthYear = earthFit1, BagEarthRandom = earthFit2, NetYear = eFit1, NetRandom = eFit2))
summary(resamps)
bwplot(resamps, layout = c(3,1))
dotplot(resamps, metric = "RMSE")
dotplot(resamps, metric = "Rsquared")
difValues <- diff(resamps)
summary(difValues)



## Output Data ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%From Freds to make a table, not sure i'm there yet
#model <- c("GBM", "sVM", "RF", "GLMN", "FDA")
#aucS <- c(gbmAUC, svmAUC, rfAUC, glmnAUC, fdaAUC)
#aucCI <- c(gbmAUCci, svmAUCci, rfAUCci, glmnAUCci, fdaAUCci)
#kappaS <- c(gbmCM$overall[2], svmCM$overall[2], rfCM$overall[2], glmnCM$overall[2], fdaCM$overall[2])
#accuracy <- c(gbmCM$overall[1], svmCM$overall[1], rfCM$overall[1], glmnCM$overall[1], fdaCM$overall[1])
#accuracyP <- c(gbmCM$overall[6], svmCM$overall[6], rfCM$overall[6], glmnCM$overall[6], fdaCM$overall[6])
#mcnemarP <- c(gbmCM$overall[7], svmCM$overall[7], rfCM$overall[7], glmnCM$overall[7], fdaCM$overall[7])

##sumData <- data.frame(model, aucS, kappaS, accuracy, accuracyP, mcnemarP)
#fileName <- paste(projectDir,'/Tuberolabium_guamense_ModelComparison.csv', sep="")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Predict Copper not Metals~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Code not done yet%%%%%%%%%%%%%%%%%%%%%
#Copper only had 4 samples that were non-detect, plus it seems to travel farther in Dust, so 
#I'm interested in its distribution relative to roads for the wetlands part of this and 
#It may be more tangible for people, so I think it's worth doing.
names(allData)
#Reduce the raw data to relevant variables and assign better names
copper<-allData[,c("Location.x","year","RoadType","PercTree","PercPaved","Paved","LandCoverCategory","Cu")]
names(copper)<-c("DistanceToRoad","Year","RoadTraffic","ForestCover","ImperviousSurface","PavedRoad","LandCover","Copper")

#deal with this nasty correlation between forest cover and impervious surface
#by transforming Impervious Surface into Unpaved Surface
#To do this subtract from 100
copper$UnpavedSurface<-100 - copper$ImperviousSurface
#Now add to the Percent Forest Cover data to give a high number as natural forested
#a mid number as natural non-forested or forested but impervious surface
#A low number is impervious and not forested - predicted high metal concentration and 
#transport

copper$UnpavedForest<-copper$UnpavedSurface+copper$ForestCover
names(copper)
str(copper)

#create lists of variables for factor and numeric assignment
CuVarListFactors<-c("RoadTraffic","Year","PavedRoad","LandCover")
CuVarListNumeric<-c("DistanceToRoad","ForestCover","ImperviousSurface","Copper")

#assign variables to factor or numeric
copper[,VarListFactors] = lapply(copper[,VarListFactors], 
                                   function(x) as.factor(as.character(x)))
copper[,CuVarListNumeric] = lapply(copper[,CuVarListNumeric], 
                                   function(x) as.numeric(as.character(x)))


hist(log(copper$Copper))

library(caret)

#We could use the createDataPartion function in caret to split the data in stratefied random way

set.seed(333)
splitDatacu <- createDataPartition(copper$Copper, p = 0.75, list = FALSE, times = 1)
randomtraincu<-copper[splitDatacu,]
randomtestcu<-copper[-splitDatacu,]


#because we replicated these samples in time, we can split the data into training and 
#testing data sets by year.Training will be 2010 (which has more transect points at 
#fewer sites) and 2011 (which has fewer points at more sites)
#I'm making them separately first, so I can combine them at will later
train2010cu<-copper[copper$Year == "2010",]
train2011cu <- copper[copper$Year == "2011",] 
#We'll see how good we are at predicting 2012!
test2012cu <- copper[copper$Year == "2012",] 

train1011cu<-rbind(train2010cu,train2011cu)

#make a list of these datasets in case I can do things in batch
datalistcu<-c("randomtraincu","train2010cu","train2011cu","train1011cu","randomtestcu","test2012cu")
trainlistcu<-c("randomtraincu","train2010cu","train2011cu","train1011cu")
testlistcu<-c("randomtestcu","test2012cu")


#now, drop year because I don't want it in the analysis - I can't get any of the following to work
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#lapply(myList, function(x) x[(names(x) %in% c("ID", "Value"))])
#list2env(lapply(lst,`[`,-1), envir=.GlobalEnv)
#list2env(lapply(datalist, function(x) x[(names(x) %in% c("DistanceToRoad","Year","RoadTraffic","ForestCover","ImperviousSurface","Paved","Metals"))]), envir=.GlobalEnv)

datalistcu<-c("randomtraincu","train2010cu","train2011cu","train1011cu","randomtestcu","test2012cu")
keepscu <- c("DistanceToRoad","RoadTraffic","LandCover","ForestCover","PavedRoad","Copper")

names(randomtraincu)

for (dat in datalistcu) {
  jnk<-get(dat)[,keepscu]
  assign(paste0(dat), jnk)
}

names(train1011cu)

#simple split with Site as a group: http://topepo.github.io/caret/data-splitting.html



#This specifies 10 fold cross validation with 10 repeats default summary for regression
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 15, returnData = TRUE, savePredictions = "final",
                           verboseIter = FALSE, returnResamp = "final" )


#Make a method list to use in a lapply statement for the train functions
models<-c("rf","gbm","earth","glmStepAIC","bagEarthGCV", "glmnet")


## Random Forests in caret 
rfgrid<-expand.grid(mtry = 1:4)
rfit3 <- train(log(Copper) ~ ., data = train1011cu, method = "rf", trControl = fitControl, tuneGrid = rfgrid, importance = T)
print(rfit3)
plot(rfit3)
summary(rfit3)

rfImp3 <- varImp(rfit3, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(rfImp3)

rfTest3 <- predict(rfit3, test2012cu)
plot(exp(rfTest3)~test2012cu$Copper, xlim = c(-1, 50), ylim = c(-1, 50))
abline(0, 1)



#Run a normal random forest model to get all the outputs in the package
?randomForest
rf3<-randomForest(log(Copper) ~ ., data = train1011cu, mtry = 2, strata = DistanceToRoad, importance = T)
summary(rf3)
plot(exp(rf3$y)~exp(rf3$predicted), xlim = c(-1, 50), ylim = c(-1, 50))
abline(0,1)
rf3$importance
hist(train1011cu$Copper)
hist(test2012cu$Copper, col = "red")
#%%%%%%%%%%%%%%%%%%%%%%%There is some wierd fake floor going on in this model and it is NOT 
#Reliable%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Boosted Regression in caret 
gbmGrid <- expand.grid(interaction.depth = (1:4), n.trees = (1:30)*30, shrinkage = 0.001,
                       n.minobsinnode = 10)
gbmFit3<-train(log(Copper) ~ ., data = train1011cu, method = "gbm", tuneGrid = gbmGrid, trControl = fitControl)
gbmFit3
plot(gbmFit3)
summary(gbmFit3$results)
gbmImp3 <- varImp(gbmFit3, scale = TRUE, useModel = TRUE)
plot(gbmImp3)

gbmTest3 <- predict(gbmFit3, test2012cu)
plot(exp(gbmTest3)~test2012cu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0,1)


#the gbm  predictions are floored and ceilinged too! - - This has to do somewhat with the shrinkage
#there's a tradeoff in number of variables that are important, and how far afield the model
#will predict. 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?gbm

gbm3<-gbm(log(Copper) ~ ., data = train1011cu)
?predict.gbm
gbm3Test<-predict.gbm(gbm3, data = test2012cu, n.trees = 100, type = "response" )
summary(gbm3)



#Partial Least Squares
plsFit3<-train(log(Copper) ~ ., data = train1011cu, method = "pls", trControl = fitControl, tuneGrid = plsgrid)
plsFit3
summary(plsFit3)
plsImp3 <- varImp(plsFit3, scale = TRUE, useModel = T)
plot(plsImp3)

plsTest3<- predict(plsFit3, test2012cu)
plot(exp(plsTest3)~test2012cu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0, 1)



#Bagged Earth models with CV pruning
earthFit3<-train(log(Copper) ~ ., data = train1011cu, method = "bagEarthGCV", trControl = fitControl)
earthFit3
summary(earthFit3)
earthImp3 <- varImp(earthFit3, scale = TRUE, useModel = TRUE)
plot(earthImp3)
earthTest3<- predict(earthFit3, test2012cu)
plot(exp(earthTest3)~test2012cu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0, 1)

#generalized linear regression stepwise feature selection 
glmstepFit3<-train(log(Copper) ~ ., data = train1011cu, method = "glmStepAIC", trControl = fitControl)
glmstepFit3
summary(glmstepFit3)
glmstepImp3<-varImp(glmstepFit3, scale = TRUE, useModel = TRUE)
plot(glmstepImp3)
GLMTest3<- predict(glmstepFit3, test2012cu)
plot(exp(GLMTest3)~test2012cu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0, 1)


#Elastic Net Regression in caret 
eFit3 <- train(log(Copper) ~ ., data = train1011cu, method = "glmnet", trControl = fitControl)
print(eFit3)
plot(eFit3)
summary(eFit3)
eFit3
eImp3 <- varImp(eFit3, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(eImp3)

eTest3 <- predict(eFit3, test2012cu)
plot(exp(eTest3)~test2012cu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0, 1)



## Random Forests in caret 
rfit4 <- train(log(Copper) ~ ., data = randomtraincu, method = "rf", trControl = fitControl, tuneGrid = rfgrid,  importance = T)
print(rfit4)
plot(rfit4)
summary(rfit4)
rfImp4 <- varImp(rfit4, scale = TRUE, useModel = FALSE)  #If usemodel = F plots baseline, e.g., linear model t values
plot(rfImp4)
rfImp4 <- varImp(rfit4, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(rfImp4)

rfTest4 <- predict(rfit4, randomtestcu)
plot(exp(rfTest4)~randomtestcu$Copper, xlim = c(-2, 20), ylim = c(-2, 20))
abline(0, 1)


##Boosted Regression in caret
gbmGrid <- expand.grid(interaction.depth = (1:4), n.trees = (1:30)*30, shrinkage = 0.01,
                       n.minobsinnode = 20)
gbmFit4<-train(log(Copper) ~ ., data = randomtraincu, method = "gbm", tuneGrid = gbmGrid, trControl = fitControl)
gbmFit4
plot(gbmFit4)
summary(gbmFit4$results)
gbmImp4 <- varImp(gbmFit4, scale = TRUE, useModel = TRUE)
plot(gbmImp4)

gbmTest4 <- predict(gbmFit4, randomtestcu)
plot(exp(gbmTest4)~randomtestcu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0,1)

#Partial Least Squares
plsFit4<-train(log(Copper) ~ ., data = randomtraincu, method = "pls", trControl = fitControl, tuneGrid = plsgrid)
plsFit4
summary(plsFit4)
plsImp4 <- varImp(plsFit4, scale = TRUE, useModel = T)
plot(plsImp4)

plsTest4<- predict(plsFit4, randomtestcu)
plot(exp(plsTest4)~randomtestcu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0, 1)



#Bagged Earth models with CV pruning
earthFit4<-train(log(Copper) ~ ., data = randomtraincu, method = "bagEarthGCV", trControl = fitControl)
earthFit4
summary(earthFit4)

earthImp4 <- varImp(earthFit4, scale = TRUE, useModel = TRUE)
plot(earthImp4)
earthTest4<- predict(earthFit4, randomtestcu)
plot(exp(earthTest4)~randomtestcu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0, 1)

#generalized linear regression stepwise feature selection 
glmstepFit4<-train(log(Copper) ~ ., data = randomtraincu, method = "glmStepAIC", trControl = fitControl)
glmstepFit4
summary(glmstepFit4)

glmstepImp4<-varImp(glmstepFit4, scale = TRUE, useModel = TRUE)
plot(glmstepImp4)
GLMTest4<- predict(glmstepFit4, randomtestcu)
plot(exp(GLMTest4)~randomtestcu$Copper, xlim = c(-2, 50), ylim = c(-2, 50))
abline(0, 1)


#Elastic Net Regression in caret 
eFit4 <- train(log(Copper) ~ ., data = randomtraincu, method = "glmnet", trControl = fitControl)
print(eFit4)
plot(eFit4)
summary(eFit4)

eImp4 <- varImp(eFit4, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(eImp4)

eTest4 <- predict(eFit4, randomtestcu)
plot(exp(eTest4)~randomtestcu$Copper, xlim = c(-2, 50), ylim = c(-2, 20))
abline(0, 1)


## Model Selection ##
#Make a model list to use in a lapply statement for the train functions
models<-c("rf","gbm","earth","glmStepAIC","bagEarthGCV", "glmnet")

resampsyear <- resamples(list(RFYear = rfit3,  GBMYear = gbmFit3, NetYear = eFit3,
                          PLSYear = plsFit3,  BagEarthYear = earthFit3, GLMYear = glmstepFit3))


resampsrandom <- resamples(list(GLMRandom = glmstepFit4, GBMRandom = gbmFit4, RFRandom = rfit4, PLSRandom = plsFit4 ,
                           BagEarthRandom = earthFit4, NetRandom = eFit4))

summary(resampsyear)
summary(resampsrandom)
bwplot(resampsyear, layout = c(3,1))
bwplot(resampsrandom, layout = c(3,1))
dotplot(resampsyear, metric = "RMSE")
dotplot(resampsrandom, metric = "RMSE")
dotplot(resampsyear, metric = "Rsquared")
dotplot(resampsrandom, metric = "Rsquared")

difValuesyear <- diff(resampsyear)
summary(difValuesyear)

difValuesrandom <-diff(resampsrandom)
summary(difValuesrandom)




## Random Forests in caret 
rfgrid<-expand.grid(mtry = 1:4)
rfitFinal <- train(Metals ~ ., data = BaseData, method = "rf", trControl = fitControl, tuneGrid = rfgrid, importance = T)
print(rfitFinal)
plot(rfitFinal)
summary(rfitFinal)
rfImpFinal <- varImp(rfitFinal, scale = TRUE, useModel = FALSE)  #If usemodel = T plots each individual variable
plot(rfImpFinal)
rfImpFinal <- varImp(rfitFinal, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(rfImpFinal)


#Run a normal random forest model to get all the outputs in the package

rfFinal<-randomForest(Metals ~ ., data = BaseData, mtry= 2, strata = DistanceToRoad, importance = T)
summary(rfFinal)
plot(rfFinal$y~rfFinal$predicted)
abline(0,1)
rfFinal$importance

##Boosted Regression in caret 
gbmGrid <- expand.grid(interaction.depth = (1:4), n.trees = (1:30)*30, shrinkage = 0.01,
                       n.minobsinnode = 10)
gbmFitFinal<-train(Metals ~ ., data = BaseData, method = "gbm", tuneGrid = gbmGrid, trControl = fitControl)
gbmFitFinal
plot(gbmFitFinal)
summary(gbmFitFinal$results)
gbmImpFinal <- varImp(gbmFitFinal, scale = TRUE, useModel = TRUE)
plot(gbmImpFinal)


#Partial Least Squares

plsFitFinal<-train(Metals ~ ., data = BaseData, method = "pls", trControl = fitControl, tuneGrid = plsgrid)
plsFitFinal
summary(plsFitFinal)
plsImpFinal <- varImp(plsFitFinal, scale = TRUE, useModel = T)
plot(plsImpFinal)


#Bagged Earth models with CV pruning
earthFitFinal<-train(Metals ~ ., data = BaseData, method = "bagEarthGCV", trControl = fitControl)
earthFitFinal
summary(earthFitFinal)
earthImpFinal <- varImp(earthFitFinal, scale = TRUE, useModel = TRUE)
plot(earthImpFinal)

#generalized linear regression stepwise feature selection 
glmstepFitFinal<-train(Metals ~ ., data = BaseData, method = "glmStepAIC", trControl = fitControl)
glmstepFitFinal
summary(glmstepFitFinal)
glmstepImpFinal<-varImp(glmstepFitFinal, scale = TRUE, useModel = TRUE)
plot(glmstepImpFinal)


#Elastic Net Regression in caret 
eFitFinal <- train(Metals ~ ., data = BaseData, method = "glmnet", trControl = fitControl)
print(eFitFinal)
plot(eFitFinal)
summary(eFitFinal)

eImpFinal <- varImp(eFitFinal, scale = TRUE, useModel = TRUE)  #If usemodel = T plots each individual variable
plot(eImpFinal)

#~~~~~~~~~~~~~~~~~~~~~Final Model Comparison~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

resampsFinal <- resamples(list(LinearModel = glmstepFitFinal, BoostedRegression = gbmFitFinal, RandomForest = rfitFinal, pls = plsFitFinal ,
                                BaggedEarth = earthFitFinal, ElasticNet = eFitFinal))

summary(resampsFinal)

bwplot(resampsFinal, layout = c(3,1))

dotplot(resampsFinal, metric = "RMSE")

dotplot(resampsFinal, metric = "Rsquared")


difValues <- diff(resampsFinal)
summary(difValues)

