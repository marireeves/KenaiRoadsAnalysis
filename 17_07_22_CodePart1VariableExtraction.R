#Mari K Reeves
#July 9, 2017
#Spatial data analysis Kenai roads

#
# Traffic data modification
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

#This is a buffer around the KPB streets file, to see if I can extract road data in R
###Needed to use GIS here and need a bigger buffer around streets to not drop transect points
#200m buffer on KPB Streets Layer
StreetBuffer<-readOGR(ShapeDir, 'StreetsNoFilterOilFieldSplit200m')

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


#reproject these smaller raster files into UTM
#This screwed up my extractions, so not going to do this
#LandCover_UTM <- projectRaster(Landcrop, crs = utmSys)
#TreeCover_UTM<- projectRaster(Treecrop, crs = utmSys)
#Pavement_UTM<- projectRaster(Pavecrop, crs = utmSys)


#plot(LandCover_UTM)
#plot(TreeCover_UTM)
#plot(Pavement_UTM)
#write them to drive, they took a bit to generate

#writeRaster(LandCover_UTM, filename = "LandcoverCrop", format="GTiff", overwrite = TRUE)
#writeRaster(TreeCover_UTM, filename = "TreecoverCrop", format="GTiff", overwrite = TRUE) 
#writeRaster(Pavement_UTM, filename = "PavementCrop", format="GTiff", overwrite = TRUE) 

#now all poly and line files in UTM zone 5
#transects_UTM <- spTransform(TransectData, utmSys)
#streets_UTM <- spTransform(RoadData, utmSys)
#allsites_UTM<-spTransform(AllSites, utmSys)
#streetbuffer_UTM<-spTransform(StreetBuffer,utmSys)
#landuse_UTM<-spTransform(LandUse, utmSys)
#traffic_UTM<- spTransform(Traffic,utmSys)


#check if that worked 
#plot(LandCover_UTM)
#plot(transects_UTM, add = T, col = "turquoise")
#plot(TreeCover_UTM, add = T)
#plot(Pavement_UTM, add = T)
#plot(streets_UTM, add = T, col = "red")
#plot(allsites_UTM, add = T, col = "green")
#plot(streetbuffer_UTM, col = "white")
#plot(landuse_UTM, add = T, col = "grey")
#plot(traffic_UTM, add = T, col = "black")

#head(transects_UTM)
#make a list of street types to retain for analysis -- Not sure I want to do this just yet
#StreetList<-c("State Highway","Town Major Collector","Town Medium Volume","National Forest Highway","Town Low Volume","Oil Field","Private Road" )

# Create Road Layer by Types relevant to analysis
#plot(streets_UTM)
#head(streets_UTM)
#names(streets_UTM)

#MyStreets_UTM <-streets_UTM[streets_UTM$SClass == StreetList,]

#plot(MyStreets_UTM, add = T, col = "red") 
#head(MyStreets_UTM)

#helpful page: http://rstudio-pubs-static.s3.amazonaws.com/7993_6b081819ba184047802a508a7f3187cb.html

#create a bigger buffer around the streets layer so it's not dropping points
#buf100_UTM<-gBuffer(MyStreets_UTM, width = 1000)
#This creates a spatial polygons, but I lose the dataframe, which is what I need to query


#clip this layer to the transects bounding box, with UTM this time
#---Warning---This step changes spatial lines dataframe to spatial lines - - dropping for now, did it in ArcGIS

#streetbox <- bbox(transects_UTM)

#Street_clip<- gClip(MyStreets_UTM, streetbox)


#plot(Street_clip)


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
proj4string(LandCover)
proj4string(TreeCover)
proj4string(Impervious)

#Yep. Assign coordinates to the transect points
MyCoords<-coordinates(transects_aea)
#check that worked
head(transects_aea)

#Join Point Information to the transect points with coordinates
TransPoints<-cbind(transects_aea[,c("ID","Location","PrimaryNam","ZN","SClass","Surface","USECODE","USAGE","OWN_TYPE")], MyCoords)

#Estract the raster data
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


#Clean up Transpoints4Export:
#make a copy with a shorter name
trans<-Transpoints4Export
#Missing data for points outside KPB Land Use polygons because they are in the road right of way
#Reclassify NAs in USECODE to "500" (arbitrary)

#Reclassify NAs in USAGE to "500 Road Right of Way"
trans$USAGE2<-as.character(trans$USAGE)
trans$USAGE2[is.na(trans$USAGE2)]<-"500 Road Right of Way"

#Reclassify NAs in OWN_TYPE to StateDOT

trans$OWN_TYPE2<-as.character(trans$OWN_TYPE)
trans$OWN_TYPE2[is.na(trans$OWN_TYPE2)]<-"StateDOT"


#Replace NA values in Zone with "REMOTE" because they are
trans$ZN<-as.character(trans$ZN)
trans$ZN[is.na(trans$ZN)]<-"REMOTE"
nrow(is.na(trans$ZN))

#drop unwanted variables
names(trans)
#[1] "ID"         "Location"   "PrimaryNam" "ZN"         "SClass"     "Surface"    "USECODE"   
#[8] "USAGE"      "OWN_TYPE"   "coords.x1"  "coords.x2"  "Value"      "PercTree"   "PercPaved" 
#[15] "Name"       "Category"   "Definition" "USECODE2"   "USAGE2"     "OWN_TYPE2" 

trans<-trans[,c("ID","Location","PrimaryNam","ZN","SClass","Surface","Value","PercTree","PercPaved",
                 "Name","Category","Definition","USAGE2","OWN_TYPE2")]

#fix a few names
names(trans)<- c("ID","Location","StreetName","Zone","RoadType","RoadSurface","LandCoverCode","PercTree","PercPaved",
                   "LandCoverName","LandCoverCategory","LandCoverDefinition","LandUse","LandOwnership")


#one last re-assignment to the RoadTypes category because what KPB calls a National Forest Highway
#is actually on the National Wildlife Refuge, and I want my figures to be correct for the paper
trans$RoadType<-as.character(trans$RoadType)
trans$RoadType[trans$RoadType == "National Forest Highway"]<-"Refuge Road"

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
allowedVars<-c("LandUseIntensity","LandUseCat")


readNewData("LUTAll.csv", allowedVars)

TransCoded<-addNewData("LUTAll.csv", trans, allowedVars)
str(TransCoded)


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

