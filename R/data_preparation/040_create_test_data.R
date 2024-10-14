#'@name 040_create_test_data.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 12-04-2024
#'@description 




# 0 - set up ####
#---------------#

library(terra)
library(dismo)
library(tidyverse)
library(stringr)
"%not_in%" <- Negate("%in%")

# 1 - artificial absence points ####
#----------------------------------#
# ticks exposed to far subzero temperatures (less than −5°C) die rapidly https://doi.org/10.1093/jme/tjaa220 
tabsm=terra::rast("data-perperation-lisa-bald/data-processed/Weather/TabsM_processed.tif")
absencePoints= lapply(2015:2021, function(year){
  tabsmYearly=terra::subset(tabsm, apply(expand.grid(sprintf("TabsM_%s", year), str_pad(1:12, 2, pad = "0")), 1, paste, collapse="_"))
  tabsmMean=terra::mean(tabsmYearly)
  # mask areas with more than 4 degrees mean annual temperature
  tabsmMean[tabsmMean > 4]<-NA
  # create absence points
  absence=as.data.frame(dismo::randomPoints(raster::stack(tabsmMean), n=10000))
  absence=sf::st_as_sf(absence, remove=F, coords=c("x", "y"), crs=sf::st_crs("epsg:2056"))
  absence$year<- year
  return(absence)
})
absencePoints=do.call(rbind, absencePoints)

if(!dir.exists( "data-perperation-lisa-bald/modeling/testdata")) dir.create("data-perperation-lisa-bald/modeling/testdata")
sf::write_sf(absencePoints, "data-perperation-lisa-bald/modeling/testdata/absence_temperature.gpkg")

# create artificial absence points form lakes
lakes=sf::read_sf("data-perperation-lisa-bald/data-raw/public/lakes/swisstlmregio_product_lv95/Hydrography/swissTLMRegio_Lake.shp")
mask=terra::rast("data-perperation-lisa-bald/data-processed/Mask/Mask_processed.tif")
lakes=sf::st_transform(lakes, terra::crs(mask))
lakes=terra::rasterize(terra::vect(lakes), mask)
lakes=terra::mask(lakes, mask)
terra::writeRaster(lakes, "data-perperation-lisa-bald/data-raw/public/lakes/lakes.tif")

# sample absence points in lakes
absence=as.data.frame(dismo::randomPoints(raster::stack(lakes), n=30000))
absence=sf::st_as_sf(absence, remove=F, coords=c("x", "y"), crs=sf::st_crs("epsg:2056"))
absence$year="lake"
sf::write_sf(absence, "data-perperation-lisa-bald/modeling/testdata/absence_lakes.gpkg")


# 2 - absence data in spatial folds ####
#--------------------------------------#
# read presence absence data
absence=sf::read_sf("data-perperation-lisa-bald/modeling/testdata/absence_temperature.gpkg")
absenceLake=sf::read_sf("data-perperation-lisa-bald/modeling/testdata/absence_lakes.gpkg")
absence=rbind(absence, absenceLake)
rm(absenceLake)
# load spatial blocks
block=readRDS("data-perperation-lisa-bald/modeling/trainingdata/spatialBlocks_bioclim2.RDS")
block=block$blocks
sf::write_sf(block, "data-perperation-lisa-bald/modeling/trainingdata/spatialBlocks_bioclim2.gpkg")
# assign spatia blocks to absence data to create spatial independent test folds
blockAbsence=blockCV::cv_spatial(x=absence, user_blocks = block, folds_column = "folds", selection="predefined", hexagon=F)
absence$fold<-blockAbsence$folds_ids
sf::write_sf(absence, "data-perperation-lisa-bald/modeling/testdata/absence.gpkg")

# 3 - create train and test datasets ####
#---------------------------------------#
absence=sf::read_sf("data-perperation-lisa-bald/modeling/testdata/absence.gpkg")
presence=sf::read_sf("data-perperation-lisa-bald/modeling/trainingdata/trainingData_biogeo2.gpkg")
#blockPresence=blockCV::cv_spatial(x=presence, user_blocks = block, folds_column = "folds", selection="predefined", hexagon=F)
#presence$fold<-blockPresence$folds_ids
#sf::write_sf(presence, "data-perperation-lisa-bald/modeling/trainingdata/trainingData_biogeo2.gpkg")

ffme=combn(unique(presence$fold),2)
# variable names
var=read.csv("data-perperation-lisa-bald/results/variable_importance_biogeo2.csv")$Variable

# seperate training and test data
for(f in 1:length(ffme[1,])){
  
  test=presence%>%dplyr::filter(fold %in% ffme[,f])%>%dplyr::mutate(occurance=1)%>%dplyr::select(c( "date","datetime","X","Y","year","month","year_month","fold","geom","occurance"))
  train=presence%>%dplyr::filter(fold %not_in% ffme[,f])
  # add absence data to test data
  test1=absence%>%dplyr::filter(fold %in% ffme[,f])%>%
    dplyr::mutate(occurance=0, X=x, Y=y, year=year, month=NA, year_month=NA, date=NA,datetime=NA)%>%
    dplyr::select(c( "date","datetime","X","Y","year","month","year_month","fold","geom","occurance"))
  
  test=rbind(test,test1);rm(test1)
  
  # save data
  if(!dir.exists(sprintf("data-perperation-lisa-bald/modeling/testdata/testFold%s", f))) dir.create(sprintf("data-perperation-lisa-bald/modeling/testdata/testFold%s", f))
  sf::write_sf(test, sprintf("data-perperation-lisa-bald/modeling/testdata/testFold%s/testData_testFold%s.gpkg",f,f))
  sf::write_sf(train, sprintf("data-perperation-lisa-bald/modeling/testdata/testFold%s/trainData_trainFold%s.gpkg",f,f))
  train=train%>%as.data.frame()%>%dplyr::mutate(x=X,y=Y)%>%dplyr::select("species","x","y","fold",all_of(var))
  write.csv(train, sprintf("data-perperation-lisa-bald/modeling/testdata/testFold%s/trainData_trainFold%s.csv",f,f), row.names = F)
  
  # create bg data
  bg=do.call(rbind,lapply(list.files("data-perperation-lisa-bald/modeling/background", full.names = T, pattern=".csv")[-1], function(x){
    data=sf::read_sf(x)
    data=data%>%dplyr::sample_n(size=1000)
    data=data%>%dplyr::select("species","x","y","fold",all_of(var))
    data$year_month<- gsub(".csv","",gsub("data-perperation-lisa-bald/modeling/background/bg_","",x))
    return(data)
  }))
  bg=bg%>%na.omit()%>%dplyr::filter(year_month %not_in% unique(test$year_month))
  bg$year_month<-NULL
  write.csv(bg, sprintf("data-perperation-lisa-bald/modeling/testdata/testFold%s/bg_trainFold%s.csv",f,f), row.names = F)
  rm(test,train,bg)
  }







