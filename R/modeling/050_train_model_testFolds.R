#'@name 060_train_model_testFolds.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 15-04-2024
#'@description create spatialMaxent Model for all combinations of test folds
#'@output Maxent model



# 0 - set up ####
#---------------#

library(parallel)
library(doParallel)

# jar file path
if(Sys.info()[4] == "pc19656"){
  spatialMaxent="java -jar D:/spatialMaxent_jar/spatialMaxent.jar"
} else if(Sys.info()[4] == "PC19664") {
  spatialMaxent="java  -jar E:/bald/BatModelingRLP/01_software/02_spatialMaxent/spatialMaxent.jar"
} else if(Sys.info()[4] == "pc19543") {
  spatialMaxent="java -jar /media/memory02/casestudies/BatModelingRLP/01_software/02_spatialMaxent/spatialMaxent.jar"
} else {
  message("please specify the path to the spatialMaxent.jar file.")
}

#"java -jar /media/memory02/casestudies/BatModelingRLP/01_software/02_spatialMaxent/spatialMaxent.jar"

# 1 - model spatialMaxent ####
#----------------------------#

# for each test fold create one model to evaluate
testfolds=list.dirs("data-perperation-lisa-bald/modeling/testdata/", recursive = F, full.names = F)
#f=testfolds[1]
for(f in testfolds){
  bg=sprintf("%s/data-perperation-lisa-bald/modeling/testdata/%s/bg_trainFold%s.csv", normalizePath(getwd()), f,gsub("testFold","",f))
  train=sprintf("%s/data-perperation-lisa-bald/modeling/testdata/%s/trainData_trainFold%s.csv", normalizePath(getwd()), f,gsub("testFold","",f))
  
  timeTest=unique(sf::read_sf(sprintf("%s/data-perperation-lisa-bald/modeling/testdata/%s/testData_%s.gpkg", normalizePath(getwd()), f,f))$year_month)
  timeTest=timeTest[!is.na(timeTest)]
  registerDoParallel(cl <- makeCluster(60))
  foreach::foreach (timeStep = timeTest)%dopar%{
    outDir=sprintf("%s/data-perperation-lisa-bald/modeling/spatialMaxent/output_testing/%s/%s", normalizePath(getwd()),f ,timeStep)
    if(!dir.exists(outDir)){
      dir.create(outDir, recursive = T)
      projectionLayers=sprintf("%s/data-perperation-lisa-bald/modeling/predictors/%s", normalizePath(getwd()), timeStep)
      layer="togglelayertype=CLC togglelayertype=swissLC"
      
      if(Sys.info()[4] == "pc19656"){
        system(sprintf(" %s cache=false ffs=false fvs=false tuneRM=false finalModel=true cvGrids=false betamultiplier=6.5 noquadratic noproduct nohinge responseCurves=true  outputdirectory=%s samplesfile=%s environmentallayers=%s warnings=false projectionLayers=%s outputGrids=false writeMESS=false writeClampGrid=false askoverwrite=false %s autorun ",
                       spatialMaxent,outDir,train,bg,projectionLayers, layer))
      } else {
        system(sprintf(" %s cache=false finalModel=true cvGrids=false  ffs=false fvs=false tuneRM=false betamultiplier=6.5 noquadratic noproduct nohinge responseCurves=true  outputdirectory=%s samplesfile=%s environmentallayers=%s warnings=false projectionLayers=%s outputGrids=false writeMESS=false writeClampGrid=false askoverwrite=false %s autorun ",
                      spatialMaxent,outDir,train,bg,projectionLayers, layer))
      }
    }# end if dir exists
  }# end for loop time
}


