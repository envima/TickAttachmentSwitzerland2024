#'@name 020_prepare_covariates.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 08-01-2024
#'@description resample all environmental variables to 100m resolution and the mask of Switzerland
#'@output "data-perperation-lisa-bald/data-processed/predictors.tif"

# 0 - set up ####
#---------------#

library(tidyverse)
library(terra)
library(stringr)


# 1 - mask ####
#-------------#
if(!file.exists("data-perperation-lisa-bald/data-processed/Mask/Mask_processed.tif")){
  # load mask of switzerland and adapt all other covariate to it
  mask=terra::rast("data-perperation-lisa-bald/data-processed/Mask/Mask.tif")
  names(mask)<-"mask"
  terra::writeRaster(mask, "data-perperation-lisa-bald/data-processed/Mask/Mask_processed.tif")
}else mask=terra::rast("data-perperation-lisa-bald/data-processed/Mask/Mask_processed.tif")

# 2 - dhm ####
#------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/DHM/DHM_processed.tif")){
  dhm=terra::rast("data-perperation-lisa-bald/data-processed/DHM/DHM25_2056.tif")%>%
    terra::resample( mask)%>%
    terra::mask( mask)
  names(dhm)<- "DHM"
  terra::writeRaster(dhm, "data-perperation-lisa-bald/data-processed/DHM/DHM_processed.tif")
}else DHM=terra::rast("data-perperation-lisa-bald/data-processed/DHM/DHM_processed.tif")


# 3 - climate data ####
#---------------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/Weather/rhiresM_processed.tif")){
  rhiresM=terra::rast(lapply(seq(2015,2021, 1), function(i){
    rhiresM<-terra::rast(sprintf("data-perperation-lisa-bald/data-processed/Weather/RhiresM/%s-01-01_%s-12-01.tif", i,i))%>%
      terra::resample(mask)%>%
      terra::mask(mask)
    names(rhiresM)<-paste("RhiresM",i, str_pad(gsub("RhiresM_","",names(rhiresM)), 2, pad = "0"), sep="_")
    return(rhiresM)
  }))
  terra::writeRaster(rhiresM, "data-perperation-lisa-bald/data-processed/Weather/rhiresM_processed.tif");gc()
} else rhiresM=terra::rast("data-perperation-lisa-bald/data-processed/Weather/rhiresM_processed.tif")

if(!file.exists("data-perperation-lisa-bald/data-processed/Weather/SrelM_processed.tif")){
  SrelM=terra::rast(lapply(seq(2015,2021, 1), function(i){
    SrelM<-terra::rast(sprintf("data-perperation-lisa-bald/data-processed/Weather/SrelM/%s-01-01_%s-12-01.tif", i,i))%>%
      terra::resample(mask)%>%
      terra::mask(mask)
    names(SrelM)<-paste("SrelM",i, str_pad(gsub("SrelM_","",names(SrelM)), 2, pad = "0"), sep="_")
    return(SrelM)
  }))
  terra::writeRaster(SrelM, "data-perperation-lisa-bald/data-processed/Weather/SrelM_processed.tif");gc()
} else SrelM=terra::rast("data-perperation-lisa-bald/data-processed/Weather/SrelM_processed.tif")


if(!file.exists("data-perperation-lisa-bald/data-processed/Weather/TabsM_processed.tif")){
  TabsM=terra::rast(lapply(seq(2015,2021, 1), function(i){
    TabsM<-terra::rast(sprintf("data-perperation-lisa-bald/data-processed/Weather/TabsM/%s-01-01_%s-12-01.tif", i,i))%>%
      terra::resample(mask)%>%
      terra::mask(mask)
    names(TabsM)<-paste("TabsM",i, str_pad(gsub("TabsM_","",names(TabsM)), 2, pad = "0"), sep="_")
    return(TabsM)
  }))
  terra::writeRaster(TabsM, "data-perperation-lisa-bald/data-processed/Weather/TabsM_processed.tif");gc()
} else TabsM=terra::rast("data-perperation-lisa-bald/data-processed/Weather/TabsM_processed.tif")


# 4 - forest data ####
#--------------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/Forest/forestMix_processed.tif")){
  forestMix=terra::rast("data-perperation-lisa-bald/data-raw/public/Forest/Waldmischungsgrad_2018_10m_2056.tif")%>%
    terra::resample( mask)
  forestMix[is.na(forestMix)]<-0
  forestMix=terra::mask(forestMix, mask)
  names(forestMix)<-"forestMix"
  terra::writeRaster(forestMix, "data-perperation-lisa-bald/data-processed/Forest/forestMix_processed.tif", overwrite=T)
} else forestMix=terra::rast("data-perperation-lisa-bald/data-processed/Forest/forestMix_processed.tif")

# 5 - CLC ####
#------------#
if(!file.exists("data-perperation-lisa-bald/data-processed/CLC/CLC_processed.tif")){
  CLC=terra::rast("data-perperation-lisa-bald/data-processed/CLC/CLC_CH_2018.tif")
  names(CLC)<-"CLC"
  terra::writeRaster(CLC, "data-perperation-lisa-bald/data-processed/CLC/CLC_processed.tif")
} else CLC=terra::rast("data-perperation-lisa-bald/data-processed/CLC/CLC_processed.tif")


# 6 - Population ####
#-------------------#
if(!file.exists("data-perperation-lisa-bald/data-processed/Population/Population_2015-2021_processed.tif")){
  population=terra::rast("data-perperation-lisa-bald/data-processed/Population/Population_2015-2021.tif")%>%
    terra::resample(mask)
  population[is.na(population)]<-0
  population= terra::mask(population, mask)
  names(population)<-paste("population",gsub("-01-01","",names(population)),  sep="_")
  terra::writeRaster(population, "data-perperation-lisa-bald/data-processed/Population/Population_2015-2021_processed.tif", overwrite=T)
} else population=terra::rast("data-perperation-lisa-bald/data-processed/Population/Population_2015-2021_processed.tif")

# 7 - spectral analysis ready data [EVI] ####
#-------------------------------------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/EVI/EVI_processed.tif")){
  EVI=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/EVI_Landsat/",recursive = T, pattern="_EVI_nanmedian.tif$", full.names = T))
  names(EVI)<- paste("EVI",gsub("_EVI_nanmedian", "",names(EVI)),sep="_") 
  EVI=terra::aggregate(EVI, fact=3)
  EVI=terra::project(EVI,terra::crs(mask))
  EVI=terra::resample(EVI,mask)
  EVI=terra::mask(EVI, mask)
  terra::writeRaster(EVI, "data-perperation-lisa-bald/data-processed/EVI/EVI_processed.tif");gc()
} else EVI=terra::rast("data-perperation-lisa-bald/data-processed/EVI/EVI_processed.tif")


# 8- LAI ####
#-----------#

if(!file.exists("data-perperation-lisa-bald/data-processed/LAI/LAI_processed.tif")){
  LAI=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/LAI/",recursive = T, pattern="_LAI_nanmedian.tif$", full.names = T))
  names(LAI)<- paste("LAI",gsub("_LAI_nanmedian", "",names(LAI)),sep="_") 
  LAI=terra::aggregate(LAI, fact=3)
  LAI=terra::project(LAI,terra::crs(mask))
  LAI=terra::resample(LAI,mask)
  LAI=terra::mask(LAI, mask)
  terra::writeRaster(LAI, "data-perperation-lisa-bald/data-processed/LAI/LAI_processed.tif")
} else LAI=terra::rast("data-perperation-lisa-bald/data-processed/LAI/LAI_processed.tif")

# 9 - GCI ####
#------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/GCI/GCI_processed.tif")){
  GCI=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/GCI/",recursive = T, pattern="_GCI_nanmedian.tif$", full.names = T))
  names(GCI)<- paste("GCI",gsub("_GCI_nanmedian", "",names(GCI)),sep="_") 
  GCI=terra::aggregate(GCI, fact=3)
  GCI=terra::project(GCI,terra::crs(mask))
  GCI=terra::resample(GCI,mask)
  GCI=terra::mask(GCI, mask)
  terra::writeRaster(GCI, "data-perperation-lisa-bald/data-processed/GCI/GCI_processed.tif")
} else GCI=terra::rast("data-perperation-lisa-bald/data-processed/GCI/GCI_processed.tif")


# 10 - Swiss land cover data ####
#-------------------------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/swissLC/swissLC_processed.tif")){
  swissLC=terra::rast("data-perperation-lisa-bald/data-raw/public/Arealstatistik/AS18_72.tif")
  names(swissLC)<- "swissLC"
  swissLC=terra::project(swissLC,terra::crs(mask), method="near")
  swissLC=terra::resample(swissLC,mask,method="near")
  swissLC=terra::mask(swissLC, mask)
  if(!dir.exists("data-perperation-lisa-bald/data-processed/swissLC/")) dir.create("data-perperation-lisa-bald/data-processed/swissLC/")
  terra::writeRaster(swissLC, "data-perperation-lisa-bald/data-processed/swissLC/swissLC_processed.tif")
} else swissLC=terra::rast("data-perperation-lisa-bald/data-processed/swissLC/swissLC_processed.tif")



# 11 - deer data ####
#-------------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/deer/deer_processed.tif")){
  deer=terra::rast("data-perperation-lisa-bald/data-raw/public/deer/tif/ensroemodel.tif")
  deer=terra::project(deer,mask)
  deer=terra::resample(deer,mask)
  deer=terra::mask(deer, mask)
  names(deer)<- "deer"
  if(!dir.exists("data-perperation-lisa-bald/data-processed/deer/")) dir.create("data-perperation-lisa-bald/data-processed/deer/")
  terra::writeRaster(deer, "data-perperation-lisa-bald/data-processed/deer/deer_processed.tif")
}else deer=terra::rast("data-perperation-lisa-bald/data-processed/deer/deer_processed.tif")


# 12 - cropland ####
#------------------#

if(!file.exists("data-perperation-lisa-bald/data-processed/cropland/cropland_processed.tif")){
  cropland=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/cropland/", full.name=T, pattern=".tif$"))
  cropland=terra::project(cropland,mask)
  cropland=terra::resample(cropland,mask)
  cropland=terra::mask(cropland, mask)
  names(cropland) <- paste("cropland", seq(2015,2022), sep="_")
  if(!dir.exists("data-perperation-lisa-bald/data-processed/cropland/")) dir.create("data-perperation-lisa-bald/data-processed/cropland/")
  terra::writeRaster(cropland, "data-perperation-lisa-bald/data-processed/cropland/cropland_processed.tif")
}else cropland=terra::rast("data-perperation-lisa-bald/data-processed/cropland/cropland_processed.tif")

# 13 - forest fraction ####
#-------------------------#


if(!file.exists("data-perperation-lisa-bald/data-processed/forest_fraction/forest_fraction_processed.tif")){
  forest_fraction=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/forest_fraction/", full.name=T, pattern=".tif$"))
  forest_fraction=terra::project(forest_fraction,mask)
  forest_fraction=terra::resample(forest_fraction,mask)
  forest_fraction=terra::mask(forest_fraction, mask)
  names(forest_fraction) <- paste("forest_fraction", seq(2015,2022), sep="_")
  if(!dir.exists("data-perperation-lisa-bald/data-processed/forest_fraction/")) dir.create("data-perperation-lisa-bald/data-processed/forest_fraction/")
  terra::writeRaster(forest_fraction, "data-perperation-lisa-bald/data-processed/forest_fraction/forest_fraction_processed.tif")
}else forest_fraction=terra::rast("data-perperation-lisa-bald/data-processed/forest_fraction/forest_fraction_processed.tif")



# 14 - human footprint ####
#-------------------------#


if(!file.exists("data-perperation-lisa-bald/data-processed/human_footprint/human_footprint_processed.tif")){
  human_footprint=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/human_footprint/", full.name=T, pattern=".tif$"))
  human_footprint=terra::project(human_footprint,mask)
  human_footprint=terra::resample(human_footprint,mask)
  human_footprint=terra::mask(human_footprint, mask)
  names(human_footprint) <- paste("human_footprint", seq(2015,2022), sep="_")
  if(!dir.exists("data-perperation-lisa-bald/data-processed/human_footprint/")) dir.create("data-perperation-lisa-bald/data-processed/human_footprint/")
  terra::writeRaster(human_footprint, "data-perperation-lisa-bald/data-processed/human_footprint/human_footprint_processed.tif")
}else human_footprint=terra::rast("data-perperation-lisa-bald/data-processed/human_footprint/human_footprint_processed.tif")



# 15 - population world ####
#--------------------------#


if(!file.exists("data-perperation-lisa-bald/data-processed/population_world/population_world_processed.tif")){
  population_world=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/population_world/", full.name=T, pattern=".tif$"))
  population_world=terra::project(population_world,mask)
  population_world=terra::resample(population_world,mask)
  population_world=terra::mask(population_world, mask)
  names(population_world) <- paste("population_world", seq(2015,2022), sep="_")
  if(!dir.exists("data-perperation-lisa-bald/data-processed/population_world/")) dir.create("data-perperation-lisa-bald/data-processed/population_world/")
  terra::writeRaster(population_world, "data-perperation-lisa-bald/data-processed/population_world/population_world_processed.tif")
}else population_world=terra::rast("data-perperation-lisa-bald/data-processed/population_world/population_world_processed.tif")



# 16 - annual snow cover ####
#---------------------------#


if(!file.exists("data-perperation-lisa-bald/data-processed/annual_snow_cover/annual_snow_cover_processed.tif")){
  annual_snow_cover=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/annual_snow_cover/", full.name=T, pattern=".tif$"))
  annual_snow_cover=terra::project(annual_snow_cover,mask)
  annual_snow_cover=terra::resample(annual_snow_cover,mask)
  annual_snow_cover=terra::mask(annual_snow_cover, mask)
  names(annual_snow_cover) <- paste("annual_snow_cover", seq(2015,2022), sep="_")
  if(!dir.exists("data-perperation-lisa-bald/data-processed/annual_snow_cover/")) dir.create("data-perperation-lisa-bald/data-processed/annual_snow_cover/")
  terra::writeRaster(annual_snow_cover, "data-perperation-lisa-bald/data-processed/annual_snow_cover/annual_snow_cover_processed.tif")
}else annual_snow_cover=terra::rast("data-perperation-lisa-bald/data-processed/annual_snow_cover/annual_snow_cover_processed.tif")



# 17 - travel time cities ####
#----------------------------#


if(!file.exists("data-perperation-lisa-bald/data-processed/travel_time_cities/travel_time_cities_processed.tif")){
  travel_time_cities=terra::rast(list.files("data-perperation-lisa-bald/data-raw/public/travel_time_cities/", full.name=T, pattern=".tif$"))
  travel_time_cities=terra::project(travel_time_cities,mask)
  travel_time_cities=terra::resample(travel_time_cities,mask)
  travel_time_cities=terra::mask(travel_time_cities, mask)
  names(travel_time_cities) <-do.call("c", lapply(names(travel_time_cities), function(x) {
    x=strsplit(x, "_")
    x=unlist(x)
    n=paste("travel_time_cities",x[6], sep="_")
    return(n)
  }))
  if(!dir.exists("data-perperation-lisa-bald/data-processed/travel_time_cities/")) dir.create("data-perperation-lisa-bald/data-processed/travel_time_cities/")
  terra::writeRaster(travel_time_cities, "data-perperation-lisa-bald/data-processed/travel_time_cities/travel_time_cities_processed.tif")
}else travel_time_cities=terra::rast("data-perperation-lisa-bald/data-processed/travel_time_cities/travel_time_cities_processed.tif")







# 18 - write one single output file ####
#--------------------------------------#
pred=terra::rast(list(CLC,DHM, forestMix, population, rhiresM, SrelM, TabsM, GCI,LAI,EVI,deer, annual_snow_cover, cropland, forest_fraction, human_footprint, population_world, swissLC, travel_time_cities))
terra::writeRaster(pred, "data-perperation-lisa-bald/data-processed/predictors.tif", overwrite=T)

