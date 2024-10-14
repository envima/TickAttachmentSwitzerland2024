#'@name 030_prepare_maps.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 24-01-2024
#'@description assign crs to all output maps


library(terra)
library(ggplot2)


# 1 - list and assign crs to all output maps ####
#-----------------------------------------------#

for(i in c("biogeo", "biogeo2", "hexagon")){
  if(!file.exists(sprintf("data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/ticks_2015_2021_epsg2056_%s.tif",i,i))){
    r=terra::rast(list.files(sprintf("data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/", i), pattern=".asc$", recursive = F, full.names=T))
    r2=terra::rast(list.files(sprintf("data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/", i), pattern="avg.asc$", recursive = T, full.names=T))
    names(r2)<-gsub("tick_","",gsub("_avg","",names(r2)))
    r=terra::rast(list(r,r2));rm(r2)
    terra::crs(r)<-"epsg:2056"
    terra::plot(r$`2015_01`)
    terra::writeRaster(r, sprintf("data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/ticks_2015_2021_epsg2056_%s.tif",i,i))
  }
}

# 2 - save maps as image ###
#--------------------------#

for(i in c("biogeo", "biogeo2", "hexagon")){
  
  r=terra::rast(sprintf("data-perperation-lisa-bald/modeling/spatialMaxent/output_%s/ticks_2015_2021_epsg2056_%s.tif",i,i))
  for(j in names(r)){
    # r2=terra::subset(r, names(r)[grepl(i, names(r))])
    r2=terra::subset(r, j)
    if(!dir.exists(sprintf("data-perperation-lisa-bald/figures/monthly/%s",i))) dir.create(sprintf("data-perperation-lisa-bald/figures/monthly/%s",i), recursive = T)
    png(filename=sprintf("data-perperation-lisa-bald/figures/monthly/%s/maps_%s.png", i, j))
    p=terra::plot(r2, range=c(0,1),nc=3,main=j, col=rev(grDevices::hcl.colors(n=50,palette = "Spectral")))
    dev.off()
    # ggplot2::ggsave(p, filename=sprintf("data-perperation-lisa-bald/figures/maps_%s.png", i))
    rm(r2)
  }
}



