#'@name 040_plot_variable_importance.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 29-02-2024
#'@description create barplot of variable importance
#'@output image [.png]

# 0 - set up ####
#---------------#

# load the library
library(forcats)
library(ggplot2)
library(tidyverse)

# 1 - plot ####
#-------------#

for (i in c("biogeo", "biogeo2", "hexagon")){
  
  vars=read.csv(sprintf("data-perperation-lisa-bald/results/variable_importance_%s.csv",i))
  
  # Reorder following the value of another column:
  varImpPlot=vars %>%
    mutate(name = fct_reorder(Variable, Percent.contribution)) %>%
    ggplot( aes(x=name, y=Percent.contribution)) +
    geom_bar(stat="identity", fill="maroon3", alpha=.6, width=.4) +
    coord_flip() +
    ylab("Percent contribution [%]") +
    theme_bw()+
    xlab("Variable")
  
  ggsave(varImpPlot, filename=sprintf("data-perperation-lisa-bald/figures/variableImportance_%s.png",i), width=4, height=4)
  
}
