getPredictors<- function(timeSteps="2016_01"){
  year=strsplit(timeSteps, "_")[[1]][1]
  month=strsplit(timeSteps, "_")[[1]][2]
  seasons=data.frame(month=str_pad(1:12, 2, pad="0"), season=c("winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "autumn", "autumn", "autumn", "winter"))
  
  
  population= terra::rast("data-perperation-lisa-bald/data-processed/Population/Population_2015-2021_processed.tif")%>%
    terra::subset( sprintf("population_%s",year))
  names(population)<-"population"
  #extract forestMix data
  forestMix<-  terra::rast("data-perperation-lisa-bald/data-processed/Forest/forestMix_processed.tif")
  # extract DHm data
  DHM<-  terra::rast("data-perperation-lisa-bald/data-processed/DHM/DHM_processed.tif")
  # extract CLC
  CLC<-  terra::rast("data-perperation-lisa-bald/data-processed/CLC/CLC_processed.tif")
  # extract weather data
  rhiresM<-terra::rast("data-perperation-lisa-bald/data-processed/Weather/rhiresM_processed.tif")%>%
    terra::subset(sprintf("RhiresM_%s", timeSteps))
  names(rhiresM)<-"rhiresM"
  SrelM<-terra::rast("data-perperation-lisa-bald/data-processed/Weather/SrelM_processed.tif")%>%
    terra::subset(sprintf("SrelM_%s", timeSteps))
  names(SrelM)<-"SrelM"
  TabsM<-terra::rast("data-perperation-lisa-bald/data-processed/Weather/TabsM_processed.tif")%>%
    terra::subset(sprintf("TabsM_%s", timeSteps))
  names(TabsM)<-"TabsM"
  # extract LAI
  LAI<-terra::rast("data-perperation-lisa-bald/data-processed/LAI/LAI_processed.tif")%>%
    terra::subset(sprintf("LAI_%s_%s", year, seasons[seasons$month == month,]$season))
  names(LAI)<-"LAI"
  # extract EVI
  EVI<-terra::rast("data-perperation-lisa-bald/data-processed/EVI/EVI_processed.tif")%>%
    terra::subset(sprintf("EVI_%s_%s", year, seasons[seasons$month == month,]$season))
  names(EVI)<-"EVI"
  # extract GCI
  GCI<-terra::rast("data-perperation-lisa-bald/data-processed/GCI/GCI_processed.tif")%>%
    terra::subset(sprintf("GCI_%s_%s", year, seasons[seasons$month == month,]$season))
  names(GCI)<-"GCI"
  
  pred=terra::rast(list(GCI,EVI,LAI,TabsM,SrelM,rhiresM,CLC, population, DHM, forestMix))
  rm(GCI,EVI,LAI,TabsM,SrelM,rhiresM,CLC, population, DHM, forestMix)
  rm(seasons);gc()
  return(pred)
}