#'@name 060_evaluation.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 24-01-2024
#'@description calculate accuracy metrics for all test data
#'@output RDS file with accuracy metrics for all test folds
#'


# 0 - set up ####
#---------------#

library(terra)
library(sf)
library(raster)
library(ecospat)
library(Metrics)
library(mecofun)
library(prg)
library(parallel)

# - 1 load test data and predictions ####
#---------------------------------------#

data=lapply(list.dirs("data-perperation-lisa-bald/modeling/testdata/", recursive=F, full.names = F), function(fold){
  
  testData=sf::read_sf(sprintf("data-perperation-lisa-bald/modeling/testdata/%s/testData_%s.gpkg", fold,fold))
  #predictionTimes=list.dirs(sprintf("data-perperation-lisa-bald/modeling/spatialMaxent/output_testing/%s", fold), recursive = F, full.names = F)
  
  data=do.call(rbind,mclapply( unique(testData$year_month)[!is.na(unique(testData$year_month))], function(i){
    prediction=terra::rast(sprintf("data-perperation-lisa-bald/modeling/spatialMaxent/output_testing/%s/%s/final/tick_%s.asc", fold,i,i))
    terra::crs(prediction)<-"epsg:2056"
    testDataSubset=testData%>%dplyr::filter(year_month==i)
    testDataAbsence=testData%>%dplyr::filter(occurance==0)%>%dplyr::filter(year %in% c("lake",  substr(i, 1,4)))%>%dplyr::slice_sample(n=nrow(testDataSubset))
    
    
    testDataSubset=rbind(testDataAbsence, testDataSubset);rm(testDataAbsence)
    
    
    testDataSubset$predict<- terra::extract(prediction, testDataSubset, ID=FALSE)%>%dplyr::pull()
    testDataSubset=testDataSubset[!is.na(testDataSubset$predict), ]
    
    # calculate continuous boyce index:
    CBI=tryCatch(
      {  CBI=ecospat::ecospat.boyce(raster::raster(prediction),
                                    testDataSubset[testDataSubset$occurance==1,], PEplot = FALSE)
      CBI=CBI$cor
      
      },
      error=function(cond) {return(NA)}
    )  
    # calculate evaluation metrics for model assesment
    PRG=prg::calc_auprg(prg::create_prg_curve(labels = testDataSubset$occurance, pos_scores = testDataSubset$predict))
    COR=cor(testDataSubset$predict, testDataSubset$occurance, method = "pearson")
    MAE=Metrics::mae(testDataSubset$occurance, testDataSubset$predict)
    m=mecofun::evalSDM(testDataSubset$occurance, testDataSubset$predict)
    logloss=Metrics::logLoss(testDataSubset$occurance, testDataSubset$predict)
    rmse=Metrics::rmse(testDataSubset$occurance, testDataSubset$predict)
    result=data.frame(year_month=i,testFold=fold,m,PRG,COR,CBI, MAE,rmse,logloss)
    return(result)
  }, mc.cores=50) #end lapply
  )# end rbind
  return(data)
})# end lapply 

results=do.call(rbind, data)
saveRDS(results, "data-perperation-lisa-bald/modeling/results/ticks_results.RDS")






