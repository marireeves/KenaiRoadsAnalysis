#Mari K Reeves
#July 9, 2017
#Spatial data analysis Kenai roads

#
# Remove all in workspace
rm(list = ls())
graphics.off()


# Read in Base Packages ---------------------------------------------------

pckg <- c('raster','rgdal','rgeos','RColorBrewer','sp','MASS','stringr',
          'glmnet', 'colorRamps', 'caret', 'plyr','dplyr', 'tidyr', 'tmap','R.utils',
          'stringr','rgdal','rgeos', 'leaflet', 'GISTools', 'forestFloor',
          'maptools','PBSmapping','gdistance', 'randomForestSRC', 'ggRandomForests',
          'MASS','scatterplot3d','colorRamps','rasterVis',
          'dismo','gbm','geosphere', 'spatialEco', 'pscl', 'OpenStreetMap', 
          'R.utils','stringr','raster','rgdal','rgeos',
          'maptools','PBSmapping','gdistance', 'curl', 'RCurl',
          'MASS','scatterplot3d','colorRamps','rasterVis',
          'lme4', 'dplyr', 'tidyr', 'survival','pROC',
         'spatialEco', 'vegan', 'zoo', 'reshape2',
          'pscl', 'randomForestSRC', 'ggplot2','kernlab',
          'coda', 'languageR', 'RColorBrewer','lmerTest','pbkrtest') 





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


# Read in non-spatial Data' -----------------------------------------------------------

# INPUT AND OUTPUT DIRECTORIES
space<-"E:/1New/RoadsAnalysis/"
AnalysisDir <- paste0(space, 'KenaiInputForEdgeEffect/')
ShapeDir <- paste0(AnalysisDir, "GIS/FinalModels")
OutputDir <- paste0(AnalysisDir, 'Analysis_Output/')
if (file.exists(OutputDir)==F){
  dir.create(OutputDir)
}
setwd(paste0(AnalysisDir, 'Analysis_Output/'))


# Data Manipulation and Lanscape Variable Extraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##Alaska is UTM zone 5 https://commons.wikimedia.org/wiki/File:UTM_Zones_AK.jpg

EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code

EPSG[grepl("UTM$", EPSG$note), ] # search for UTM code

#set the system to UTM zone 5
utmSys<-"+proj=utm +zone=5 +ellps=WGS84"


# READING IN SPATIAL DATA ---------------------------------------------------------
list.files(ShapeDir, pattern='\\.shp$')
#Pull in the GPS Transect information 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Need to post this to GIT)
#I did the point to polygon extraction in ArcGIS to get the road and land use information 
# attached to these points before I brought them in

#These are soil sample point locations - joined to the data from the closest road in ArcGIS
TransectData <- readOGR(ShapeDir, 'Transects4EditsOwnUseStreetInfo')

#Files from Kenai Peninsula Borough Website (http://www.kpb.us/gis-dept/kpb-data-downloads)
#made some modifications to the KPB files in ArcGIS, described below

#This is the KPB Streets layer, which I modified by digitizing the network of gravel roads on the 
#Swanson River Oil and Gas Fields at the north end of the Kenai National Wildlife Refuge
RoadData <- readOGR(ShapeDir, 'AllStreetsOilFieldSplit')

#These are wetlands surveyed for abnormal amphbians, not relevant to this study other than site selection
AllSites<- readOGR(ShapeDir, 'AllSitesCurrentHistoricAnchorage')

#This is a buffer around the KPB streets file, to randomly generate points in for prediction
###Needed to use GIS to make the buffer how I wanted it
#80m buffer on KPB Streets Layer
StreetBuffer<-readOGR(ShapeDir, 'MyStreetsOilFieldSplit80mBuffer')

#KPB Land Use Polygon, which I modified to alter 3 polygons on the Kenai Refuge from "Vacant" 
#to "Industrial" land use because they contained the oil and gas fields at Swanson River and Beaver Creek
LandUse<-readOGR(ShapeDir, 'KPBLandUseOilFieldsReclassified')

#This is a data set I obtained from the Alaska Department of Transportation of measured Traffic Data from 2011
Traffic<-readOGR(ShapeDir, 'ADOTRoadTraffic2011')

#Downloaded landcover, tree cover, and impervious layers from National Land Cover Dataset
#here (https://www.mrlc.gov/nlcd11_data.php)

#...Categorized definitions as in this table for this Analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% post on git
NLCD_definitions <- read.csv(paste0(ShapeDir, "/NLCD_definitions.csv"))

LandCover<-raster(paste0(ShapeDir,"/ak_nlcd_2011_landcover_1_15_15.img"))
TreeCover<-raster(paste0(ShapeDir,"/nlcd2011_usfs_coastal_alaska_canopy_analytical.img"))
Impervious<-raster(paste0(ShapeDir,"/ak_nlcd_2011_zone8_impervious_1_15_15.img"))


################Check Projections and reproject some files
#Make a list of files so that I can apply the same functions to all of them (doesn't work)


proj4string(TransectData)
proj4string(RoadData)
proj4string(AllSites)
proj4string(StreetBuffer)
proj4string(LandUse)
proj4string(Traffic)


#These reproject into Albers Equal Area to match Raster data, based on CRS for landcover


#myShps<-c('TransectData','RoadData','AllSites','StreetBuffer','LandUse','Traffic')
#lapply(myShps, 
#       function(x) (spTransform(x,CRSobj = CRS(proj4string(LandCover)))))

transects_aea <- spTransform(TransectData, CRSobj = CRS(proj4string(LandCover)))
streets_aea <- spTransform(RoadData, CRSobj = CRS(proj4string(LandCover)))
allsites_aea<-spTransform(AllSites, CRSobj = CRS(proj4string(LandCover)))
streetbuffer_aea<-spTransform(StreetBuffer, CRSobj = CRS(proj4string(LandCover)))
landuse_aea<-spTransform(LandUse, CRSobj = CRS(proj4string(LandCover)))
traffic_aea<- spTransform(Traffic, CRSobj = CRS(proj4string(LandCover)))


#Clip the Raster Layers, they're too big to process

#################Cropping the Raster layers

#Define the extent of the crop by clicking on the plot - - commenting out because it's not repeatable
#But keeping in in case others find it useful!

#plot(LandCover)
#plot(transects_aea, col= "red", add = T)
#cropbox <- drawExtent()


#Helpful Tutorial and function for bounding box...
#http://robinlovelace.net/r/2014/07/29/clipping-with-r.html

b <- bbox(landuse_aea)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

cropbox <- gClip(landuse_aea, b)
#Warning message:(this is normal, per tutorial)
  #In RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, drop_lower_td, unaryUnion_if_byid_false,  :
      #                  spgeom1 and spgeom2 have different proj4 strings

#Create bounding box for TransectData

Landcrop<-crop(LandCover, cropbox)
Treecrop<-crop(TreeCover, cropbox)
Pavecrop<-crop(Impervious, cropbox)
plot(Landcrop)
plot(Treecrop)
plot(Pavecrop)


#Helpful tutorial: http://spatial.ly/wp-content/uploads/2013/12/intro-spatial-rl-3.pdf


##Need to extract covariate data for the transect points and be happy with these predictors
##Join the road data back in to the transect points - Not working. I'm going with what I did in GIS
#TransPoints<-raster::intersect(transects_UTM, streetbuffer_UTM)


##Extract Land Cover, impervious, and Tree Cover Pixels at transect locations
#can't think of a problem doing this in Alaska Albers, since I'm not really measuring anything
#other than the buffers, and I think that will be ok. The UTM transformation is screwing this up
#which is why I'm doing it in aea.

#quickly check projections are the same for all layers
proj4string(transects_aea)
proj4string(streetbuffer_aea)
proj4string(LandCover)
proj4string(TreeCover)
proj4string(Impervious)

#Yep. Assign coordinates to the transect points
MyCoords<-coordinates(transects_aea)
#check that worked
head(transects_aea)

#crop the streetbuffer to the study area by hand
plot(Landcrop)
plot(transects_aea, add=T)
cropbox2 <- drawExtent()

#This is repeatable, but cut off too much road area, so ultimately drew cropbox by hand
#a <- bbox(transects_aea)

#gClip <- function(shp, bb){
#  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
#  else b_poly <- as(extent(bb), "SpatialPolygons")
#  gIntersection(shp, b_poly, byid = T)
#}

#cropbox2 <- gClip(transects_aea, a)

streetcrop80mbuffer_aea<-crop(streetbuffer_aea, cropbox2)
plot(streetcrop80mbuffer_aea)
plot(transects_aea, add = T, col = "red")


#~~~~~~~~~~~~~~~~~~Generate the Random Points for Model Predictions 
#~~~~~~~~~~~~~~~~~~generate random points within this 80 m buffer
#~~~~~~~~~~~~~~~~And extract spatial variables at each point for later analysis

#~~~~~~~~~~Filter the Roads to only include the road types I used to develop my models
#~~~~~~~~~Did this in ArcGIS so it was done on the input file. 



#Randomly sample these buffer polygons to pick a million points within them (or 50K...)
library(sp)

Buffer80_MyStreetsRandom<-spsample(streetcrop80mbuffer_aea, 50000, type = "random")

plot(Buffer80_MyStreetsRandom, add= T, col = "red")


##Join the road data back in to the points
Buffer80RandomPoints<-raster::intersect(Buffer80_MyStreetsRandom, streetcrop80mbuffer_aea)



#####################Extract data from the land and tree cover rasters######

##for the random points
head(Buffer80RandomPoints)
names(Buffer80RandomPoints)
coord<-coordinates(Buffer80RandomPoints)
Buffer80RandomPoints<-cbind(Buffer80RandomPoints[,1:2], coord)

##Extract Land Cover and Tree Cover Pixels at unbuffered transect locations
Buffer80RandomPoints$Value <- raster::extract(LandCover, Buffer80RandomPoints) 
head(Buffer80RandomPoints)


Buffer80RandomPoints$PercTree <- raster::extract(TreeCover, Buffer80RandomPoints) 

Buffer80RandomPoints$PercPaved <- raster::extract(Impervious, Buffer80RandomPoints) 

head(Buffer80RandomPoints)
Random4Export<-left_join(Buffer80RandomPoints@data, NLCD_definitions)
#Export predictors in a csv file
write.csv(Random4Export, file = "Random4Export.csv")
#Export transect points with extracted raster data as a shp file
writeOGR(Buffer80RandomPoints, ShapeDir, "PredictorVariablesRandomPointsFinalFinal",  driver="ESRI Shapefile")

test<-Random4Export

#Join Transect Point Information to the transect points with coordinates
TransPoints<-cbind(transects_aea[,c("ID","Location","PrimaryNam","ZN","SClass","Surface","USECODE","USAGE","OWN_TYPE")], MyCoords)

#Estract the raster data for the transect points
#https://www.rdocumentation.org/packages/raster/versions/2.5-8/topics/extract

TransPoints$Value <- raster::extract(LandCover, TransPoints,  method='simple', buffer=NULL,  cellnumbers=FALSE,  fun=NULL, na.rm=FALSE, df=FALSE, factors=TRUE) 

TransPoints$PercTree <- raster::extract(TreeCover, TransPoints, method='simple', cellnumbers=FALSE,  fun=NULL, na.rm=FALSE,  df=FALSE, factors=TRUE) 

TransPoints$PercPaved<-raster::extract(Impervious, TransPoints, method='simple', cellnumbers=FALSE, fun=NULL, na.rm=FALSE,  df=FALSE, factors=TRUE) 


head(TransPoints)
Transpoints4Export<-left_join(TransPoints@data, NLCD_definitions)
#Export predictors in a csv file
write.csv(TransPoints, file = "TransPoints.csv")
#Export transect points with extracted raster data as a shp file
writeOGR(TransPoints, ShapeDir, "PredictorVariablesTransPointsUnbuffered",  driver="ESRI Shapefile")


#Clean up Transpoints4Export and Test Data Files:
#make a copy with a shorter name
trans<-Transpoints4Export

test<-Random4Export

#Do they match?
names(trans)
names(test)
head(trans)
head(test)


#No, make them match
#Drop variables not in analysis from trans:
trans<-trans[,c("ID","Location","SClass","Surface",
              "USECODE","USAGE","OWN_TYPE","coords.x1","coords.x2","Value",
                "PercTree","PercPaved","Name","Category","Definition")]


#Missing data for points outside KPB Land Use polygons because they are in the road right of way
#Reclassify NAs in USECODE to "500" (arbitrary)

#Reclassify NAs in USAGE to "500 Road Right of Way"
trans$USAGE2<-as.character(trans$USAGE)
trans$USAGE2[is.na(trans$USAGE2)]<-"500 Road Right of Way"

#Didn't extract land use data from polygons from test points AND didn't end up using this
#land use variable in final models, so not doing this step for the test points.
#This applies to next 5 lines of code, too.

#Reclassify NAs in OWN_TYPE to StateDOT

trans$OWN_TYPE2<-as.character(trans$OWN_TYPE)
trans$OWN_TYPE2[is.na(trans$OWN_TYPE2)]<-"StateDOT"


 
#fix a few names in trans
names(trans)<- c("ID","Location","RoadType","RoadSurface","USECODE","USAGE","OWN_TYPE","X","Y","LandCoverCode","PercTree","PercPaved",
                   "LandCoverName","LandCoverCategory","LandCoverDefinition","LandUse","LandOwnership")


#Fix names in test
names(test)

names(test)<- c("RoadType","RoadSurface","X","Y","LandCoverCode","PercTree","PercPaved",
                 "LandCoverName","LandCoverCategory","LandCoverDefinition")

#one last re-assignment to the RoadTypes category because what KPB calls a National Forest Highway
#is actually on the National Wildlife Refuge, and I want my figures to be correct for the paper
trans$RoadType<-as.character(trans$RoadType)
trans$RoadType[trans$RoadType == "National Forest Highway"]<-"Refuge Road"

test$RoadType<-as.character(test$RoadType)
test$RoadType[test$RoadType == "National Forest Highway"]<-"Refuge Road"


#I need to make the lookup table to classify variables in excel. 
#Exporting data so this is a cut and paste step from pivot tables.
write.csv(trans, "trans4lookup.csv")

#Make a new category, "LANDUSE" where we shorten these names for analysis
#use my LookUpTableFunction!!! Yeay!
#I modified this function from here:https://nicercode.github.io/blog/2013-07-09-modifying-data-with-lookup-tables/


##This readNewData function is going to the working directory to find the csv file for the
##lookup table. You don't load it into R first. 



##' Utility function to read/process newDataFileName for addNewData
##' 
##' @param newDataFileName name of lookup table
##' @param allowedVars vector of permissible variable names for newVariable
##' @return data.frame with columns c(lookupVariable,lookupValue,newVariable,newValue,source)
readNewData <- function(newDataFileName, allowedVars){
  
  if( file.exists(newDataFileName)){
    import <- read.csv(newDataFileName, header=TRUE, stringsAsFactors=FALSE,
                       strip.white=TRUE)
    if( nrow(import)> 0 ){
      
      #Check columns names for import are right
      expectedColumns<- c("lookupVariable","lookupValue","newVariable","newValue")
      nameIsOK <-  expectedColumns %in% names(import)
      if(any(!nameIsOK))
        stop("Incorrect name in lookup table for ",
             newDataFileName, "--> ", paste(expectedColumns[!nameIsOK],
                                            collapse=", "))
      
      #Check values of newVariable are in list of allowed variables
      import$lookupVariable[import$lookupVariable == ""] <- NA
      nameIsOK <- import$newVariable %in% allowedVars
      if(any(!nameIsOK))
        stop("Incorrect name(s) in newVariable column of ",
             newDataFileName, "--> ", paste(import$newVariable[!nameIsOK],
                                            collapse=", "))
    } else {
      import <- NULL
    }
  } else {
    import <- NULL
  }
  import
}

##' Modifies 'data' by adding new values supplied in newDataFileName
##'
##' newDataFileName is expected to have columns 
##' c(lookupVariable,lookupValue,newVariable,newValue,source)
##' 
##' Within the column 'newVariable', replace values that
##' match 'lookupValue' within column 'lookupVariable' with the value
##' newValue'.  If 'lookupVariable' is NA, then replace *all* elements
##' of 'newVariable' with the value 'newValue'.
##'
##' Note that lookupVariable can be the same as newVariable.
##'
##' @param newDataFileName name of lookup table
##' @param data existing data.frame
##' @param allowedVars vector of permissible variable names for newVariable
##' @return modified data.frame
##' 
addNewData <- function(newDataFileName, data, allowedVars){
  
  import <- readNewData(newDataFileName, allowedVars)
  
  if( !is.null(import)){    
    for(i in seq_len(nrow(import))){  #Make replacements
      col.to <- import$newVariable[i] 
      col.from <- import$lookupVariable[i]
      if(is.na(col.from)){ # apply to whole column
        data[col.to] <- import$newValue[i]
      } else { # apply to subset
        rows <- data[[col.from]] == import$lookupValue[i]
        data[rows,col.to] <- import$newValue[i]
      }
    }   
  }      
  data
}


#Assign allowed varaibles for LUT functions
allowedVars<-c("LandUseIntensity", "LandUseCat", "RoadType","Paved")
allowedVars2<-c("PavedRoad", "LandCover4Predict")

readNewData("LUTAll.csv", allowedVars)
readNewData("LUTAll2.csv", allowedVars2)


names(trans)
TransCoded<-addNewData("LUTAll.csv", trans, allowedVars)
str(TransCoded)
names(TransCoded)


TestCoded<-addNewData("LUTAll2.csv", test, allowedVars2)
str(TestCoded)


#Look at factors not analyzed prior to see if there are any I want to reclassify
#for predictions, rather than eliminate

#make a list of variables to turn into factors
TestList<-c("RoadType", "RoadSurface", "LandCoverName", "LandCoverCategory")
# Convert data to factors
lapply(TestCoded[,TestList],  function(x) levels(x))

lapply(TestCoded[,TestList],  function(x) as.factor(as.character(x)))


#The safest thing is to drop these categories that were not in analysis of field data
#When it comes to making predictions...When I tried doing this after the spatial sample, 
#I went from 56K to less than 10K points. Oops. So need to do it before on the shp file
#which may be harder.  I did it in GIS, so this file  was buffered and imported with correct
#road type

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Why still showing all these if I filtered in GIS?
#$RoadType
#[1] "Driveway"             "Legal Trail"          "Not a Road"          
#[4] "Private Road"         "Proposed"             "Refuge Road"         
#[7] "State Highway"        "Town Arterial"        "Town Low - Seasonal" 
#[10] "Town Major Collector" "Town Medium Volume"  

#Running this as a precaution

#reassign Oil Field to Private Road

TestCoded$RoadType[TestCoded$RoadType == "Oil Field" ]<-"Private Road"
#Not doing this step, it reduces the dataset to 10K records and I don't know why, so dropping
#RoadsToKeep<-c("Private Road", "Refuge Road","State Highway","Town Major Collector","Town Medium Volume")
#test_roads<-test[test$RoadType == RoadsToKeep,]

#$LandCoverCategory
#[1] "Barren"       "Crop"         "Developed"    "Forest"       "Herbaceous"   "Ice and Snow"
#[7] "NoData"       "Open Water"   "Shrub"        "Wetland"     
#%%%%%%%%%%%%%%%%%%%not working here, do in Arc

LandCoverToKeep<-c("Developed","Forest", "Shrub","Wetland")
#For some reason, this only pulls 13,457 records, when there should be many more...so
#TestCodedCover<-TestCoded[TestCoded$LandCoverCategory == LandCoverToKeep,]
Shrub<-TestCoded[TestCoded$LandCoverCategory == "Shrub",]
Wetland<-TestCoded[TestCoded$LandCoverCategory == "Wetland",]
Forest<-TestCoded[TestCoded$LandCoverCategory == "Forest",]
Developed<-TestCoded[TestCoded$LandCoverCategory == "Developed",]

TestCodedCover<-rbind(Shrub, Wetland, Forest, Developed)
#So that was the clunkiest way to do that, but it worked. Let's see if I can predict test
#now in these models

RoadsToKeep<-c("Private Road", "Refuge Road","State Highway","Town Major Collector","Town Medium Volume")
#test_roads<-test[test$RoadType == RoadsToKeep,]

##Export the random point file for analysis
library(sp)

###Needs to be TestCodedCoverHere, not this other file.....
#Useful Website

# points from scratch
#coords = cbind(x, y)

coords<-cbind(TestCodedCover$X, TestCodedCover$Y)
sp = SpatialPoints(coords)
# make spatial data frame
#spdf = SpatialPointsDataFrame(coords, data)
#spdf = SpatialPointsDataFrame(sp, data)
TestCoded4Export<-SpatialPointsDataFrame(sp, TestCodedCover)
proj4string(TestCoded4Export)<-CRS(proj4string(LandCover))

  
writeOGR(TestCoded4Export, ShapeDir, "Buffer80RandomPointsFinalFinalFinalNoReally", overwrite_layer = T,  driver="ESRI Shapefile")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#I'm sending this to GIS to generate a distance to road field there
#Then, plan to re-import that file and clean it up to run the models and predict 
#metals contamination at these points. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Bring in the Metals Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Start with the data file online 


URL<-"http://datadryad.org/bitstream/handle/10255/dryad.151900/DryadRevisedAnalyticResults.v2.csv?sequence=3"
x <- getURL(URL, ssl.verifypeer = FALSE)
chem <- read.csv(textConnection(x))


#Verify this worked
head(chem[1:6])

dim(chem)



#make a new date field that R can recognize 
chem$date<-as.POSIXct(chem$Date, tz = "", format = "%m/%d/%Y") 
chem$year<-format(chem$date, '%Y') 


#replace non-detect data with half of the sample specific detection limit
chem$newvalue<-as.numeric(chem$Value)
chem$newvalue[is.na(chem$newvalue)]<-chem$LOD[is.na(chem$newvalue)]/2


write.csv(chem, file = "chem.csv")
##Select only soil data (this dataset includes multiple media types)

soil<-chem[chem$Media == "Soil", ]


#make a list of variables to turn into factors
VariableList<-c("Site", "Date", "Location", "Analyte", "year")
# Convert data to factors
soil[,VariableList] = lapply(soil[,VariableList],  function(x) as.factor(as.character(x)))



# Data Analysis -----------------------------------------------------------
#~~~~~~~~~~~Flatten soil ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Aggregate newvalue (which is half LOD subbed in for samples below LOD) by site, location, and year

#locationmeans<-aggregate(soil$Value~soil$Analyte+soil$Location, FUN=mean, na.omit = TRUE)
#This works but is not what I want.

##Trying reshape2 package instead

##example code ##data_wide <- dcast(olddata_long, subject + sex ~ condition, value.var="measurement")
##data_wide

##the code below worked. It allowed me to be more specific than I could figure out how to be with spread

soilflat<-dcast(soil, Site + Date + Location ~ Analyte, value.var = "newvalue", fun = mean)

write.csv(soilflat, file = "flatsoildatahalfloddupmean.csv")

##Clean it up further to only retain elements of interest based on detection in at least 80% of samples
#and concerns about road relatedness. This is 
str(soilflat)
names(soilflat)


#[1] "Site"     "Date"     "Location" "Ag"       "Al"       "As"       "Ba"       "Be"       "Ca"      
#[10] "Cd"       "Co"       "Cr"       "Cu"       "Fe"       "K"        "Mg"       "Mn"       "Mo"      
#[19] "Na"       "Ni"       "Pb"       "Sb"       "Se"       "Si"       "Th"       "Tl"       "U"       
#[28] "V"        "Zn"   
Analytes2Keep<-c( "Site","Date","Location","Ca","Cr","Cu","Fe","Mg","Mn","Na","Ni","Pb","V","Zn")

BaseData<-soilflat[ , Analytes2Keep]

BaseData$date<-as.POSIXct(BaseData$Date, tz = "", format = "%m/%d/%Y") 

BaseData$year<-format(BaseData$date, '%Y') 


##Merge with predictor variables
BaseData$ID<-paste0(BaseData$Site,"_",BaseData$Location)

BaseData$ID4RanEf<-paste0(BaseData$Site,"_",BaseData$Location,"_",BaseData$year)

BaseData2<-left_join(BaseData, TransCoded, by="ID")



BaseData<-BaseData2
rm(BaseData2)

##Generate the response variables
#Create a helpful elements vector of Ca and Mg - I averaged the logged, indexed values
##GOOD - high and positive number of Good are GOOD (lots of buffering capacity)
BaseData$good<-((scale(log(BaseData$Ca)))+(scale(log(BaseData$Mg))))/2

good.v<-BaseData$good
#Create a nasty toxic elements vector - dropped Cd due to excessive non-detect rate.
##BAD - high and positive numbers of Bad are BAD (lots of toxic trace elements)
##The BAD - Cu, Na, Pb, Ni, Fe, Mn, Cr, 
#Does not include Ba, which was important variable but prob local origin)

BaseData$bad<-((scale(log(BaseData$Na)))+(scale(log(BaseData$Cr)))+
                 (scale(log(BaseData$Cu)))+(scale(log(BaseData$Ni)))+(scale(log(BaseData$Pb)))+(scale(log(BaseData$Mn)))
               +(scale(log(BaseData$V)))+(scale(log(BaseData$Zn))) +(scale(log(BaseData$Fe))))/9

bad.v<-BaseData$bad
###Good2Bad - Here I subtract Bad from Good, making high and positive numbers of this index GOOD (more buffering 
##capacity than toxic trace elements, but not in a linear way)
BaseData$good2bad<-BaseData$good-BaseData$bad
good2bad.v<-BaseData$good2bad
#Dataset is now together. No dups. No NAs. We're ready to go!!

write.csv(BaseData, "17_07_25_FinalVariables4Model.csv")

############~~~~~~~~~~~~~~~~~Run Comparative Models~~~~~~~~~~~~~~~~~~~~~~~~


#Run and compare models in caret: https://www.jstatsoft.org/article/view/v028i05
#It will ask you to restart R. Just say yes. Doesn't always go perfectly. Watch this step.
install.packages("caret", dependencies = c("Depends", "Suggests"))
#then it will stop, so run this. I am doing it separately, so that it can suggest package dependencies when needed
library(caret)

rm(TestData)
#Read in the shp file of the points extracted in last steps and edited in GIS to 
#generate a distance to road field in meters so that we can run the final models
#and predict to these points. For some reason that export/import blows away the 
#variable reclassification I did above, so I'm just re-doing it Below.
TestData <- readOGR(ShapeDir, 'Buffer80RandomPointsFinalFinalFinalDistNoReallyFinal')
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% post on git
NLCD_definitions <- read.csv(paste0(ShapeDir, "/NLCD_definitions.csv"))
FinalData<-left_join(TestData@data, NLCD_definitions)
names(FinalData)
FinalData<-FinalData[,c("x","y","Distance","SClass","Surface","PercTree","PercPaved","Category")]
names(FinalData)<-c("x","y","DistanceToRoad","RoadTraffic","RoadSurface","ForestCover","ImperviousSurface","LandCover")

head(FinalData)

#Generate the Paved binomial Variable and reassign Oil Field to Private Road.
#These functions are in code part 1 and lookup table is on Git

#Fixing this error by reclassifying these as wetlands, thinking land cover would influence 
#contaminant transport in a similar way
#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#factor LandCover has new levels Barren, Crop, Herbaceous, Open Water

#Assign allowed varaibles for LUT functions
allowedVars2<-c("RoadType", "PavedRoad", "LandCover4Predict")


readNewData("LUTAll2.csv", allowedVars2)

TestCoded<-addNewData("LUTAll2.csv", FinalData, allowedVars2)
str(TestCoded)

#reassign Oil Field to Private Road
TestCoded$RoadTraffic[TestCoded$RoadTraffic == "Oil Field" ]<-"Private Road"

#reassign "National Forest Highway" to "Refuge Road"#
TestCoded$RoadTraffic[TestCoded$RoadTraffic == "National Forest Highway" ]<-"Refuge Road"


names(TestCoded)

#drop original land cover category - using reclassified instead
TestCoded<-TestCoded[,c("x","y","DistanceToRoad","RoadTraffic","RoadSurface","ForestCover",
                        "ImperviousSurface","PavedRoad","LandCover4Predict")]
names(TestCoded)<-c("x","y","DistanceToRoad","RoadTraffic","RoadSurface","ForestCover",
                   "ImperviousSurface","PavedRoad","LandCover")

#create lists of variables for factor and numeric assignment
VarListFactors<-c("RoadTraffic","PavedRoad","LandCover")
VarListNumeric<-c("DistanceToRoad","ForestCover","ImperviousSurface")

#assign variables to factor or numeric
TestCoded[,VarListFactors] = lapply(TestCoded[,VarListFactors], 
                                    function(x) as.factor(as.character(x)))
TestCoded[,VarListNumeric] = lapply(TestCoded[,VarListNumeric], 
                                    function(x) as.numeric(as.character(x)))

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
VarListFactors2<-c("RoadTraffic","Year","PavedRoad","LandCover")
VarListNumeric2<-c("DistanceToRoad","ForestCover","ImperviousSurface","Metals")

#assign variables to factor or numeric
BaseData[,VarListFactors2] = lapply(BaseData[,VarListFactors2], 
                                    function(x) as.factor(as.character(x)))
BaseData[,VarListNumeric2] = lapply(BaseData[,VarListNumeric2], 
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
library(caret)
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




#Generate Model Objects from the Full Dataset and Generate Predictions for the 50K Points
#Reload caret, in case you want to skip to the punchline
#Run and compare models in caret: https://www.jstatsoft.org/article/view/v028i05
#It will ask you to restart R. Just say yes. Doesn't always go perfectly. Watch this step.
install.packages("caret", dependencies = c("Depends", "Suggests"))
#then it will stop, so run this. I am doing it separately, so that it can suggest package dependencies when needed
library(caret)

names(BaseData)
#Dropping ForestCover and keeping LandCover for final models instead
keeps <- c("DistanceToRoad","RoadTraffic","ImperviousSurface","LandCover","PavedRoad","Metals")
BaseData<-BaseData[,keeps]

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

#~~~~~~~~~~~~~~~~~~~~~Final Model Comparison for Weights~~~~~~~~~~~~~~~~~~~~~~~~~

resampsFinal <- resamples(list(LinearModel = glmstepFitFinal, BoostedRegression = gbmFitFinal, RandomForest = rfitFinal, PartialLeastSquares = plsFitFinal ,
                               BaggedEarth = earthFitFinal, ElasticNet = eFitFinal))

summary(resampsFinal)

bwplot(resampsFinal, layout = c(3,1))

dotplot(resampsFinal, metric = "RMSE")

dotplot(resampsFinal, metric = "Rsquared")


difValues <- diff(resampsFinal)
summary(difValues)


#~~~~~~~~~~~~~~Generate predictions from these models for the 50K point dataset

names(BaseData)
names(TestCoded)
str(TestCoded)
#remove NAs prior to analysis
TestCoded<-TestCoded[complete.cases(TestCoded),]


rfpredict<-predict(rfitFinal, TestCoded)
gbmpredict<-predict(gbmFitFinal, TestCoded)
plspredict<-predict(plsFitFinal, TestCoded)
earthpredict<-predict(earthFitFinal, TestCoded)
netpredict<-predict(eFitFinal, TestCoded)
glmpredict<-predict(glmstepFitFinal, TestCoded)


predictions<-data.frame(cbind(TestCoded, rfpredict, gbmpredict, plspredict, earthpredict, netpredict, glmpredict))
write.csv(predictions, file = "predictions.csv")
names(predictions)


