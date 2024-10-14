#'@name 010_download_new_variables.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 08-01-2024


# 1 - download environmental layers ####
#--------------------------------------#

data=read.csv("data-perperation-lisa-bald/data-raw/public/mood_layers1km.csv")
data$name<- do.call("c", lapply(data$filename, function(x) {
  x=strsplit(x, "/")
  x=unlist(x)
  n=x[length(x)]
  n=gsub("_tif",".tif",gsub("[.]","_", n))
  return(n)
}))

data=data[data$class != "land_surface_temp_nigh",]

# Specify URL where file is stored 

lapply(1:nrow(data), function(x){
  print(x)
  url=data$filename[x]
  destination=sprintf("data-perperation-lisa-bald/data-raw/public/%s/%s", data$class[x], data$name[x])
  if(!dir.exists(sprintf("data-perperation-lisa-bald/data-raw/public/%s", data$class[x]))) dir.create(sprintf("data-perperation-lisa-bald/data-raw/public/%s", data$class[x]))
  if(!file.exists(destination)){
    download.file(url=url, destfile=destination,  mode = "wb")
  } 
})