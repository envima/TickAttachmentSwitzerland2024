#'@name 020_project_models.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 08-02-2024
#'@description create spatialMaxent maps for 2015 to 2021 monthly
#'@output Maxent model
#'


# 0 - set up ####
#---------------#

library(parallel)
library(doParallel)

# jar file path
if(Sys.info()[4] == "PC19656"){
  spatialMaxent="java -cp D:/spatialMaxent_jar/spatialMaxent.jar  density.Project"
} else if(Sys.info()[4] == "PC19664") {
  spatialMaxent="java  -cp E:/bald/BatModelingRLP/01_software/02_spatialMaxent/spatialMaxent.jar density.Project"
} else {
  message("please specify the path to the spatialMaxent.jar file.")
}

#load r libraries
library(terra)
library(stringr)
library(tidyverse)


# 1 -load predictor variables ####
#--------------------------------#

df=readRDS("data-perperation-lisa-bald/data-processed/variables_temporal_resolution.RDS")
seasons=data.frame(month=str_pad(1:12, 2, pad="0"), season=c("winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "autumn", "autumn", "autumn", "winter"))


# 1 - project spatialMaxent ####
#------------------------------#

for(mod in c("biogeo", "biogeo2", "hexagon")){
  
  modelPath=sprintf("E:/bald/Switzerland_ticks/data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/model/final/tick.lambdas",mod)
  #mod=readLines(modelPath)
  
  
  
  time=expand.grid(month=str_pad(1:12, 2, pad="0"), year=seq(2015,2021))
  time$timeStep<-paste(time$year, time$month, sep="_")
  # write predictor variables as .grd to predict with spatialMaxent
  # define variables first
  
  registerDoParallel(cl <- makeCluster(30))
  foreach::foreach(timeStep=time$timeStep, .packages=c("tidyverse", "terra", "sf","stringr" , "dplyr", "magrittr"), .export = c("seasons", "df") ) %dopar% {
    
    
    if(!dir.exists(sprintf("data-perperation-lisa-bald/modeling/predictors/%s", timeStep))){
      df2=df%>%dplyr::filter(year %in% c(strsplit(timeStep, "_")[[1]][1], "static"))%>%
        dplyr::filter(month %in% c(strsplit(timeStep, "_")[[1]][2], NA))%>%
        dplyr::filter(season %in% c(seasons[seasons$month == strsplit(timeStep, "_")[[1]][2], ]$season, NA))
      # subset variable stack
      pred=raster::stack("data-perperation-lisa-bald/data-processed/predictors.tif")
      predSubset=raster::subset(pred, df2$variable)
      names(predSubset)<- gsub(paste0("_", unique(df2$season)[!is.na(unique(df2$season))]),"",gsub(paste0("_",strsplit(timeStep, "_")[[1]][1]),"",gsub(paste0("_",strsplit(timeStep, "_")[[1]][2]),"",names(predSubset))))
      dir.create(sprintf("data-perperation-lisa-bald/modeling/predictors/%s", timeStep), recursive = T)
      raster::writeRaster(raster::stack(predSubset), bylayer=T,overwrite=T,format="raster",
                          filename = sprintf("data-perperation-lisa-bald/modeling/predictors/%s/%s.asc", timeStep, names(predSubset))
      )
    }
    outDir=sprintf("%s/data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/%s", normalizePath(getwd()),mod, timeStep)
    if(!file.exists(outDir)){
      projectionLayers=sprintf("%s/data-perperation-lisa-bald/modeling/predictors/%s", normalizePath(getwd()), timeStep)
      
      if(Sys.info()[1] == "Windows"){
        system(sprintf(" %s %s %s %s writeClampGrid=false ", spatialMaxent,modelPath,projectionLayers, outDir))
      }
    }# end if dir exists
    
    #unlink(sprintf("data-perperation-lisa-bald/modeling/predictors/%s", timeStep), recursive = T)
    
  }
  
}

