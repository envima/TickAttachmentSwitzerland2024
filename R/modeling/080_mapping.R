#'@name 080_mapping.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 29-02-2024
#'@description create maps for the manuscript
#'@output image [.png]




# 0 - set up ####
#---------------#


library(terra)
library(ggplot2)
library(tidyterra)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)

#  1 - load all maps ####
#-----------------------#
r=terra::rast(list.files("data-perperation-lisa-bald/modeling/spatialMaxent/output_biogeo2/", full.names = T, pattern = ".asc"))
terra::crs(r)<-"epsg:2056"
terra::writeRaster(r, "data-perperation-lisa-bald/modeling/results/maps_tick.tif")

# 2 - map of overall risk ####
#----------------------------#
risk=sum(r)
terra::writeRaster(risk, "data-perperation-lisa-bald/modeling/results/overall_risk.tif")

# map risks yearly

for(i in 2015:2021){
  m=terra::subset(r, names(r)[grep(i, names(r))])
  risk=sum(m)
  names(risk)<-paste0("risk",i)
  terra::writeRaster(risk, sprintf("data-perperation-lisa-bald/modeling/results/risk%s.tif",i))
  rm(m,risk)
}


# 3 - yearly maps ####
#---------------------#

r=terra::rast("data-perperation-lisa-bald/modeling/results/maps_tick.tif")

for(i in 2015:2021){
  r2=terra::subset(r, names(r)[grepl("2015", names(r))])
  n=apply(expand.grid(c("January", "February", "March", "April", "May", "June", "July", "August",
                        "September", "October", "November", "December"), i), 1, paste, collapse=" ")
  
  names(r2)<- n
  #r2=tidyterra::rename_with(.data=r, .fn=n, .cols = everything())
  
  
 p=ggplot() +
   geom_spatraster(data = r2) +
   facet_wrap(~lyr, ncol = 4) +
   scale_fill_whitebox_c(
     palette = "muted",
     limits=c(0,1),
     labels = scales::label_number(suffix = ""),
     n.breaks = 12,
     guide = guide_legend(reverse = TRUE)
   ) + theme_minimal() + ylim(1040000, 1296000)+
   labs(
     fill = "",
     title = "Risk of tick attachment to humans",
     #subtitle = 
   )+
   annotation_north_arrow(
     which_north = TRUE,
     pad_x = unit(0.8, "npc"),
     pad_y = unit(0.75, "npc"),
     height = unit(0.5, "cm"),
     width = unit(0.5, "cm"),
     style = north_arrow_fancy_orienteering(text_size =0, text_col = "white",)
   ) +
   annotation_scale(
     height = unit(0.055, "npc"),
     # bar_cols="black",
     width_hint = 0.5,
     pad_x = unit(0.07, "npc"),
     pad_y = unit(0.07, "npc"),
     text_cex = .7
   )
 
  
  ggsave(p, filename=sprintf("data-perperation-lisa-bald/figures/map_%s.jpg",i), dpi=300, width = 10, height=8.5)
  
  rm(p,r2,n)
}

# 4 - map paper ####
#------------------#

r=terra::rast("data-perperation-lisa-bald/modeling/results/maps_tick.tif")
i="2015"
r2=terra::subset(r, names(r)[grepl(i, names(r))])
n=apply(expand.grid(c("January", "February", "March", "April", "May", "June", "July", "August",
                      "September", "October", "November", "December"), i), 1, paste, collapse=" ")

names(r2)<- n
#r2=tidyterra::rename_with(.data=r, .fn=n, .cols = everything())

# plot for one year with facet wrap
yearlyPlot=ggplot() +
  geom_spatraster(data = r2) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_whitebox_c(
    palette = "muted",
    limits=c(0,1),
    labels = scales::label_number(suffix = ""),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) + theme_minimal() + ylim(1040000, 1296000)+
  labs(
    fill = "",
    title = "Risk of tick attachment to humans",
    #subtitle = 
  )+
  annotation_north_arrow(
    which_north = TRUE,
    pad_x = unit(0.8, "npc"),
    pad_y = unit(0.75, "npc"),
    height = unit(0.5, "cm"),
    width = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering(text_size =0, text_col = "white",)
  ) +
  annotation_scale(
    height = unit(0.055, "npc"),
   # bar_cols="black",
    width_hint = 0.5,
    pad_x = unit(0.07, "npc"),
    pad_y = unit(0.07, "npc"),
    text_cex = .7
  )

# plot for overall risk
risk=terra::rast("data-perperation-lisa-bald/modeling/results/overall_risk.tif")

# plot for one year with facet wrap
riskPlot=ggplot() +
  geom_spatraster(data = risk) +
 # facet_wrap(~lyr, ncol = 4) +
  scale_fill_whitebox_c(
    palette = "muted",
  #  limits=c(0,1),
    labels = scales::label_number(suffix = ""),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) + theme_minimal() + ylim(1040000, 1296000)+
  labs(
    fill = "",
    title = "Overall risk of tick attachment to humans 2015 to 2021",
    #subtitle = 
  )+
  annotation_north_arrow(
    which_north = TRUE,
    pad_x = unit(0.8, "npc"),
    pad_y = unit(0.75, "npc"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(
    height = unit(0.015, "npc"),
    width_hint = 0.5,
    pad_x = unit(0.07, "npc"),
    pad_y = unit(0.07, "npc"),
    text_cex = .8
  )


p = ggarrange(riskPlot, yearlyPlot, labels=c("a)", "b)"), ncol=1)

ggsave(p, filename=sprintf("data-perperation-lisa-bald/figures/map_paper3.tiff"), dpi=600, width = 6, height=8.5)

# 5 - plot seperatetly (single maps) ####
#---------------------------------------#


r=terra::rast("data-perperation-lisa-bald/modeling/results/maps_tick.tif")

for(i in 2015:2021){
  r2=terra::subset(r, names(r)[grepl(i, names(r))])
  n=apply(expand.grid(c("January", "February", "March", "April", "May", "June", "July", "August",
                        "September", "October", "November", "December"), i), 1, paste, collapse=" ")
  
  names(r2)<- n
  #r2=tidyterra::rename_with(.data=r, .fn=n, .cols = everything())
  
  for (image in names(r2)){
    r3=terra::subset(r2, image)
    p=ggplot() +
      geom_spatraster(data = r3) +
      # facet_wrap(~lyr, ncol = 4) +
      scale_fill_whitebox_c(
        palette = "muted",
        limits=c(0,1),
        labels = scales::label_number(suffix = ""),
        n.breaks = 12,
        guide = guide_legend(reverse = TRUE)
      ) + theme_minimal() +
      labs(
        fill = "",
        title = "Risk of tick attachment to humans",
        subtitle = image
      )
    
    ggsave(p, filename=sprintf("data-perperation-lisa-bald/figures/monthly/new_figures/map_%s.png",gsub(" ","_",image)), dpi=300, width = 10, height=8)
  }
  
}
