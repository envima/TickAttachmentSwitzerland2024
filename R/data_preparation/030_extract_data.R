#'@name 030_extract_data.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 08-01-2024
#'@description extract data from the predictors at each month and year and create space time folds
#'@output geopackage and csv file with tick reports with spatial and temporal independent cv folds

# 0 - set up ####
#---------------#

library("sf")
library("terra")
library("tidyverse")
library("CAST")
library("blockCV")
library("spThin")

# 1 - clean data and variable names ####
#--------------------------------------#

data=sf::read_sf("data-perperation-lisa-bald/data-processed/Ticks/tick_reports.gpkg")
data$year<- lubridate::year(data$date)
data$month<-  str_pad(lubridate::month(data$date), 2, pad = "0")   
data$year_month<- paste(data$year, data$month, sep="_")
data=data%>%dplyr::filter(year < lubridate::year("2022-01-01"))
seasons=data.frame(month=str_pad(1:12, 2, pad="0"), season=c("winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "autumn", "autumn", "autumn", "winter"))


# load all predictor variables
pred=terra::rast("data-perperation-lisa-bald/data-processed/predictors.tif")
df=data.frame(variable=names(pred))
df$year<- do.call("c",lapply(names(pred),function(x){x=strsplit(x, "_");x=unlist(x);year=x[x %in% seq(2010,2023)];year[length(year) == 0]<- NA_character_;return(year)}))
# create column with variables that include a month
df$month<- do.call("c", lapply(names(pred),function(x){x=strsplit(x, "_");x=unlist(x);month=x[x %in% unique(data$month)];month[length(month) == 0]<- NA_character_;return(month)}))
# create column for variables that include seasons
df$season<- do.call("c",lapply(names(pred),function(x){x=strsplit(x,"_");x=unlist(x);season=x[x %in% unique(seasons$season)];season[length(season) == 0]<- NA_character_;return(season)}))
# add yearly value for continous variables
df[is.na(df$year),]$year <- "static"
# filter to 2015 to 2021
df=df%>%dplyr::filter(year %in% c(seq(2015, 2021), "static"))

saveRDS(df, "data-perperation-lisa-bald/data-processed/variables_temporal_resolution.RDS")

# 2 - extract presence only and background data ####
#--------------------------------------------------#


dataExtracted<- lapply(unique(data$year_month), function(i){
  # subset data to one month and year
  dataSubset=data%>%dplyr::filter(year_month == i)
  df2=df%>%dplyr::filter(year %in% c(as.character(unique(dataSubset$year)), "static"))
  df2=df2%>%dplyr::filter(month %in% c(unique(dataSubset$month), NA))
  df2=df2%>%dplyr::filter(season %in% c(seasons[seasons$month == unique(dataSubset$month), ]$season, NA))
  # extract the infiormaiton from the environmental covariates
  predSubset=terra::subset(pred, df2$variable)
  extr=terra::extract(predSubset, dataSubset, ID=FALSE)
  names(extr)<-gsub(paste0("_", unique(df2$season)[!is.na(unique(df2$season))]),"",gsub(paste0("_",unique(dataSubset$year)),"",gsub(paste0("_",i),"",names(extr))))
  dataSubset=cbind(dataSubset, extr) 
  
  # remove duplicates/ points on the same pixel and clean data
  extr$duplicate<- duplicated(extr)
  dataSubset=dataSubset[extr$duplicate==FALSE,]
  dataSubset=na.omit(dataSubset)
  
  #3 - create background data
  #bg=predicts::backgroundSample(mask=pred, n=10000)
  # create 10,000 random background data for each timestep
  if(!file.exists(sprintf("data-perperation-lisa-bald/modeling/background/bg_%s.csv", i))){
    bg=as.data.frame(dismo::randomPoints(raster::stack(predSubset), 10000))
    bg=sf::st_as_sf(bg, remove=F, coords=c("x" ,"y"), crs=sf::st_crs("epsg:2056"))
    extrBg=terra::extract(predSubset,bg, ID=F)
    names(extrBg)<- names(extr)[-length(extr)]
    bg$species<-"tick"
    bg$fold=1
    if(!dir.exists("data-perperation-lisa-bald/modeling/background")) dir.create("data-perperation-lisa-bald/modeling/background", recursive=T)
    bg=cbind(bg, extrBg) # add values from environmental covariates to background data
    bg=na.omit(bg) # clean background data
    sf::write_sf(bg, sprintf("data-perperation-lisa-bald/modeling/background/bg_%s.gpkg", i))
    bg=bg%>%as.data.frame()%>%dplyr::select("species","x","y","fold", names(extr)[-length(extr)])
    write.csv(bg,sprintf("data-perperation-lisa-bald/modeling/background/bg_%s.csv", i), row.names=F)
  }
  # finish writing background data next return presence only data
  # ---------------
  rm(bg,extr,extrBg,predSubset, df2)
  return(dataSubset)
})


# 2- clean and save data ####
#---------------------------#

dataExtracted=do.call(rbind, dataExtracted)
sf::write_sf(dataExtracted, "data-perperation-lisa-bald/data-processed/Ticks/tick_reports_extracted.gpkg");rm(data,seasons)

# 3 - create spatial blocks with biogeographic regions ####
#---------------------------------------------------------#
data=sf::read_sf("data-perperation-lisa-bald/data-processed/Ticks/tick_reports_extracted.gpkg")
set.seed(110124)

bioclimaticRegions=sf::read_sf("data-perperation-lisa-bald/data-raw/public/biogeografische_regionen/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp")

#create spatial blocks using the blockCV package
block=blockCV::cv_spatial(x=data, flat_top=F, user_blocks = bioclimaticRegions,plot=F)
if(!dir.exists("data-perperation-lisa-bald/modeling/trainingdata")) dir.create("data-perperation-lisa-bald/modeling/trainingdata")
saveRDS(block, "data-perperation-lisa-bald/modeling/trainingdata/spatialBlocks.RDS")
# assigning spatial IDs to presence records
data$spaceVar<-block$folds_ids#;rm(block)
#create temporal spatial folds using the CAST package
xMax=0
for(i in 1:1000){
  spaceTimeFolds=CAST::CreateSpacetimeFolds(data, spacevar = "spaceVar", timevar = "month", k=k)$indexOut
  x=sum(unlist(lapply(spaceTimeFolds, function(x){length(x)})))
  message(paste("The total number of presence records for modeling is:", x))
  if(x > xMax){
    xMax<- x
    spaceTimeFoldsMax=spaceTimeFolds
  };rm(x)
}

sum(unlist(lapply(spaceTimeFoldsMax, function(x){length(x)})))

# choose data independetn spatially and temporally
trainingData=do.call(rbind,lapply(1:k, function(x){
  data2=data%>%dplyr::slice(spaceTimeFoldsMax[[x]])%>%dplyr::mutate(fold=x, species="tick")
  return(data2)
}))

# save data as gpkg and csv
if(!dir.exists("data-perperation-lisa-bald/modeling/trainingData")) dir.create("data-perperation-lisa-bald/modeling/trainingdata", recursive = T)
sf::write_sf(trainingData, "data-perperation-lisa-bald/modeling/trainingData/trainingData_biogeo.gpkg")
bg=read.csv("data-perperation-lisa-bald/modeling/background/bg_2015_03.csv")
trainingData=trainingData%>%dplyr::mutate(x=X, y=Y)%>%dplyr::select(-c("X", "Y"))
trainingData=trainingData%>%as.data.frame()%>%dplyr::select(colnames(bg))
write.csv(trainingData, "data-perperation-lisa-bald/modeling/trainingData/trainingData_biogeo.csv", row.names=F)

# 4 - create blocks with bio regions large ####
#--------------------------------------------#
data=sf::read_sf("data-perperation-lisa-bald/data-processed/Ticks/tick_reports_extracted.gpkg")
set.seed(110124)

bioclimaticRegions=sf::read_sf("data-perperation-lisa-bald/data-raw/public/biogeografische_regionen/BiogeographischeRegionen/n2020_revision_biogeoregion_large_regions.gpkg")

#create spatial blocks using the blockCV package
block=blockCV::cv_spatial(x=data, flat_top=F, user_blocks = bioclimaticRegions,plot=F)
if(!dir.exists("data-perperation-lisa-bald/modeling/trainingdata")) dir.create("data-perperation-lisa-bald/modeling/trainingdata")
saveRDS(block, "data-perperation-lisa-bald/modeling/trainingdata/spatialBlocks_bioclim2.RDS")
# assigning spatial IDs to presence records
data$spaceVar<-block$folds_ids#;rm(block)
#create temporal spatial folds using the CAST package
k=5
xMax=0
for(i in 1:1000){
  spaceTimeFolds=CAST::CreateSpacetimeFolds(data, spacevar = "spaceVar", timevar = "month", k=k)$indexOut
  x=sum(unlist(lapply(spaceTimeFolds, function(x){length(x)})))
  message(paste("The total number of presence records for modeling is:", x))
  if(x > xMax){
    xMax<- x
    spaceTimeFoldsMax=spaceTimeFolds
  };rm(x)
}

sum(unlist(lapply(spaceTimeFoldsMax, function(x){length(x)})))

# choose data independetn spatially and temporally
trainingData=do.call(rbind,lapply(1:k, function(x){
  data2=data%>%dplyr::slice(spaceTimeFoldsMax[[x]])%>%dplyr::mutate(fold=x, species="tick")
  return(data2)
}))

# save data as gpkg and csv
if(!dir.exists("data-perperation-lisa-bald/modeling/trainingData")) dir.create("data-perperation-lisa-bald/modeling/trainingdata", recursive = T)
sf::write_sf(trainingData, "data-perperation-lisa-bald/modeling/trainingData/trainingData_biogeo2.gpkg")
bg=read.csv("data-perperation-lisa-bald/modeling/background/bg_2015_03.csv")
trainingData=trainingData%>%dplyr::mutate(x=X, y=Y)%>%dplyr::select(-c("X", "Y"))
trainingData=trainingData%>%as.data.frame()%>%dplyr::select(colnames(bg))
write.csv(trainingData, "data-perperation-lisa-bald/modeling/trainingData/trainingData_biogeo2.csv", row.names=F)


# 5 - data with spatial blocks ####
#---------------------------------#

data=sf::read_sf("data-perperation-lisa-bald/data-processed/Ticks/tick_reports_extracted.gpkg")
set.seed(110124)

#bioclimaticRegions=sf::read_sf("data-perperation-lisa-bald/data-raw/public/biogeografische_regionen/BiogeographischeRegionen/n2020_revision_biogeoregion_large_regions.gpkg")

#create spatial blocks using the blockCV package
block=blockCV::cv_spatial(x=data, flat_top=F, k=5,plot=T, size=75000)
if(!dir.exists("data-perperation-lisa-bald/modeling/trainingdata")) dir.create("data-perperation-lisa-bald/modeling/trainingdata")
saveRDS(block, "data-perperation-lisa-bald/modeling/trainingdata/spatialBlocks_hexagon.RDS")
# assigning spatial IDs to presence records
data$spaceVar<-block$folds_ids#;rm(block)
#create temporal spatial folds using the CAST package
k=5
xMax=0
for(i in 1:1000){
  spaceTimeFolds=CAST::CreateSpacetimeFolds(data, spacevar = "spaceVar", timevar = "month", k=k)$indexOut
  x=sum(unlist(lapply(spaceTimeFolds, function(x){length(x)})))
  message(paste("The total number of presence records for modeling is:", x))
  if(x > xMax){
    xMax<- x
    spaceTimeFoldsMax=spaceTimeFolds
  };rm(x)
}

sum(unlist(lapply(spaceTimeFoldsMax, function(x){length(x)})))

# choose data independetn spatially and temporally
trainingData=do.call(rbind,lapply(1:k, function(x){
  data2=data%>%dplyr::slice(spaceTimeFoldsMax[[x]])%>%dplyr::mutate(fold=x, species="tick")
  return(data2)
}))

# save data as gpkg and csv
if(!dir.exists("data-perperation-lisa-bald/modeling/trainingData")) dir.create("data-perperation-lisa-bald/modeling/trainingdata", recursive = T)
sf::write_sf(trainingData, "data-perperation-lisa-bald/modeling/trainingData/trainingData_hexagon.gpkg")
bg=read.csv("data-perperation-lisa-bald/modeling/background/bg_2015_03.csv")
trainingData=trainingData%>%dplyr::mutate(x=X, y=Y)%>%dplyr::select(-c("X", "Y"))
trainingData=trainingData%>%as.data.frame()%>%dplyr::select(colnames(bg))
write.csv(trainingData, "data-perperation-lisa-bald/modeling/trainingData/trainingData_hexagon", row.names=F)





# 5 - background data ####
#------------------------#



# 2 - background file one for 2015-2021 ####
#------------------------------------------#

bg=do.call(rbind,lapply(list.files("data-perperation-lisa-bald/modeling/background", full.names = T, pattern=".csv$"), function(x){
  data=read.csv(x)
  data=data%>%dplyr::sample_n(size=1000)
  return(data)
}))

write.csv(bg, "data-perperation-lisa-bald/modeling/background/background.csv", row.names = F)







# end script