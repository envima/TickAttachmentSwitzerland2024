#'@name 010_train_models.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 06-02-2024
#'@description create spatialMaxent Model with all data until 2021 and spatio-temporal-cv
#'@output Maxent model
#'


# 0 - set up ####
#---------------#

library(parallel)
library(doParallel)

# jar file path
if(Sys.info()[4] == "PC19656"){
  spatialMaxent="java -jar D:/spatialMaxent_jar/spatialMaxent.jar"
} else if(Sys.info()[4] == "PC19664") {
  spatialMaxent="java  -jar E:/bald/BatModelingRLP/01_software/02_spatialMaxent/spatialMaxent.jar"
} else {
  message("please specify the path to the spatialMaxent.jar file.")
}

#load r libraries
library(terra)
library(stringr)


# 1 -train maxent model ####
#--------------------------#

for(mod in c("biogeo", "biogeo2","hexagon")){
  # load filepaths for files used for modeling
  bg=sprintf("%s/data-perperation-lisa-bald/modeling/background/background.csv", normalizePath(getwd()))
  train=sprintf("%s/data-perperation-lisa-bald/modeling/trainingdata/trainingData_%s.csv", normalizePath(getwd()), mod)
  outDir=sprintf("%s/data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/model", normalizePath(getwd()),mod)
  if(!dir.exists(outDir)){
    dir.create(outDir, recursive = T)
    #  projectionLayers=sprintf("%s/data-perperation-lisa-bald/modeling/predictors/%s", normalizePath(getwd()), timeStep)
    layer=" togglelayertype=CLC togglelayertype=swissLC "
    
    if(Sys.info()[1] == "Windows"){
      system(sprintf(" %s cache=false finalModel=true cvGrids=false responseCurves=true threads=44 outputdirectory=%s samplesfile=%s environmentallayers=%s warnings=false outputGrids=false writeMESS=false writeClampGrid=false askoverwrite=false %s autorun ",
                     spatialMaxent,outDir,train,bg, layer))
    } else {
      system(sprintf(" %s cache=false finalModel=true cvGrids=false  responseCurves=true threads=60 outputdirectory=%s samplesfile=%s environmentallayers=%s warnings=false outputGrids=false writeMESS=false writeClampGrid=false askoverwrite=false %s autorun ",
                     spatialMaxent,outDir,train,bg, layer))
    }
  }# end if dir exists
}




