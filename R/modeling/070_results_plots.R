#'@name 070_results_plots.R
#'@author Lisa bald [bald@staff.uni-marburg.de]
#'@date 29-02-2024
#'@description boxplot of evaluation results
#'@output image [.png]


# 0 - set up & load data ####
#---------------------------#

library(tidyverse)
library(ggplot2)
library(viridis)
library(hrbrthemes)
# read data
data=readRDS("data-perperation-lisa-bald/modeling/results/ticks_results.RDS")
data$year=substr(data$year_month, start=0, stop=4)
data$month=substr(data$year_month, start=6, stop=10)

# 1 - create boxplots monthly ####
#--------------------------------#

for( i in c("AUC","TSS","PCC","PRG","COR","CBI","MAE","rmse","logloss")){
  
  # Boxplot basic
  p=data %>%
    ggplot( aes(x=month, y=!! rlang::sym(i), fill=month)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    #theme_ipsum() +
    theme(
      legend.position="none"#,
      # plot.title = element_text(size=11)
    ) +
    #ggtitle("Logloss") +
    xlab("Months")
  assign(i,p);rm(p)
}

p=gridExtra::grid.arrange(AUC, PRG, PCC, TSS, CBI, COR, MAE, logloss, rmse)
ggsave(p, filename="data-perperation-lisa-bald/figures/results_monthly.png", dpi=300, width = 10, height=8)
rm(p, AUC, PRG, PCC, TSS, CBI, COR, MAE, logloss, rmse,i)
# 2 - create boxplots monthly ####
#--------------------------------#

for( i in c("AUC","TSS","PCC","PRG","COR","CBI","MAE","rmse","logloss")){
  
  # Boxplot basic
  p=data %>%
    ggplot( aes(x=year, y=!! rlang::sym(i), fill=year)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    #theme_ipsum() +
    theme(
      legend.position="none"#,
      # plot.title = element_text(size=11)
    ) +
    #ggtitle("Logloss") +
    xlab("Year")
  assign(i,p);rm(p)
}

p=gridExtra::grid.arrange(AUC, PRG, PCC, TSS, CBI, COR, MAE, logloss, rmse)
ggsave(p, filename="data-perperation-lisa-bald/figures/results_yearly.png", dpi=300, width = 10, height=8)
rm(p, AUC, PRG, PCC, TSS, CBI, COR, MAE, logloss, rmse,i)


# 3 - mean values for each metric ####
#------------------------------------#

# 222 test sets in total (6 test folds, each several time stamps)


results_mean=data%>%
  summarise(AUC=round(mean(AUC), digits=2),TSS=round(mean(TSS),2),Kappa=round(mean(Kappa),2),
            Sens=round(mean(Sens),2),Spec=round(mean(Spec),2),PCC=round(mean(PCC),2),
            PRG=round(mean(PRG),2),COR=round(mean(COR),2),CBI=round(mean(CBI, na.rm=T),2),
            MAE=round(mean(MAE),2),rmse=round(mean(rmse),2),logloss=round(mean(logloss),2),
            testPoints=round(mean(testPoints),2))

results_median=data%>%
  summarise(AUC=round(median(AUC), digits=2),TSS=round(median(TSS),2),Kappa=round(median(Kappa),2),
            Sens=round(median(Sens),2),Spec=round(median(Spec),2),PCC=round(median(PCC),2),
            PRG=round(median(PRG),2),COR=round(median(COR),2),CBI=round(median(CBI, na.rm=T),2),
            MAE=round(median(MAE),2),rmse=round(median(rmse),2),logloss=round(median(logloss),2),
            testPoints=round(median(testPoints),2))


results_year_month=data%>%
  group_by(year_month) %>%
  summarise(AUC=round(mean(AUC), digits=2),TSS=round(mean(TSS),2),Kappa=round(mean(Kappa),2),
            Sens=round(mean(Sens),2),Spec=round(mean(Spec),2),PCC=round(mean(PCC),2),
            PRG=round(mean(PRG),2),COR=round(mean(COR),2),CBI=round(mean(CBI, na.rm=T),2),
            MAE=round(mean(MAE),2),rmse=round(mean(rmse),2),logloss=round(mean(logloss),2),
            testPoints=round(mean(testPoints),2))


results_year=data%>%
  group_by(year) %>%
  summarise(AUC=round(median(AUC), digits=2),TSS=round(median(TSS),2),Kappa=round(median(Kappa),2),
            Sens=round(median(Sens),2),Spec=round(median(Spec),2),PCC=round(median(PCC),2),
            PRG=round(median(PRG),2),COR=round(median(COR),2),CBI=round(median(CBI, na.rm=T),2),
            MAE=round(median(MAE),2),rmse=round(median(rmse),2),logloss=round(median(logloss),2)#,
           # testPoints=round(median(testPoints),2)
           )

results_month=data%>%
  group_by(month) %>%
  summarise(AUC=round(median(AUC), digits=2),TSS=round(median(TSS),2),Kappa=round(median(Kappa),2),
            Sens=round(median(Sens),2),Spec=round(median(Spec),2),PCC=round(median(PCC),2),
            PRG=round(median(PRG),2),COR=round(median(COR),2),CBI=round(median(CBI, na.rm=T),2),
            MAE=round(median(MAE),2),rmse=round(median(rmse),2),logloss=round(median(logloss),2)#,
            #testPoints=round(median(testPoints),2)
            )

#write.csv(results_mean, "data-perperation-lisa-bald/modeling/results/ticks_2015_2021_results_mean.csv")
write.csv(results_month, "data-perperation-lisa-bald/modeling/results/ticks_2015_2021_results_month.csv")
write.csv(results_year, "data-perperation-lisa-bald/modeling/results/ticks_2015_2021_results_year.csv")
#write.csv(results_year_month, "data-perperation-lisa-bald/modeling/results/ticks_2015_2021_results_year_month.csv")


# 4 - plot variable importance ####
#---------------------------------#

vars=read.csv("data-perperation-lisa-bald/modeling/results/variable_importance.csv")


# load the library
library(forcats)

# Reorder following the value of another column:
varImpPlot=vars %>%
  mutate(name = fct_reorder(Variable, Percent.contribution)) %>%
  ggplot( aes(x=name, y=Percent.contribution)) +
  geom_bar(stat="identity", fill="maroon3", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Percent contribution [%]") +
  theme_bw()+
  xlab("Variable")

ggsave(varImpPlot, filename="data-perperation-lisa-bald/figures/variableImportance.png", width=4, height=4)

