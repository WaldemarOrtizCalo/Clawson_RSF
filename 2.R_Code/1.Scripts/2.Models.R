#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-04-12 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(ggplot2)
library(tidyverse)
library(lares)
library(reshape2)
library(magrittr)
library(MuMIn)
#      Functions                                                            ####

#      Data                                                                 ####

# Data 
deer_north <- read_csv("1.Data/CleanData/deer_north_final.csv")
deer_south <- read_csv("1.Data/CleanData/deer_south_final.csv")

# Making Choice a factor
deer_north$choice <- factor(deer_north$choice, levels = c("0","1"))
deer_south$choice <- factor(deer_south$choice, levels = c("0","1"))

###############################################################################
#   [Covariate Exploration]                                                 ####
#      [North]                                                              ####
#        [Correlation Plots]                                                ####

north_cor_bargraph <-corr_cross(deer_north[3:10], rm.na = T, max_pvalue = 0.05, 
                          top = 10, grid = T)
north_cor_bargraph

ggsave(filename = "north_corplot.png",
       plot = north_cor_bargraph,
       device = "png",
       path = "3.Outputs/Models_ExploratoryPlots")

#        [Box-Whisker Plots]                                                ####

boxwhisk_data <- pivot_longer(deer_north,cols = starts_with("proportion"))

north_boxwhisk <- ggplot(boxwhisk_data, aes(x = name,y = value, fill = choice))+
  geom_boxplot() 

ggsave(filename = "north_boxwhisk.png",
       plot = north_boxwhisk,
       device = "png",
       path = "3.Outputs/Models_ExploratoryPlots",
       width = 10,
       height = 6,
       units = "in"
       )


#      [South]                                                              ####
#        [Correlation Plots]                                                ####

south_cor_bargraph <-corr_cross(deer_south[3:ncol(deer_south)], rm.na = T, max_pvalue = 0.05, 
                                top = 10, grid = T)
south_cor_bargraph

ggsave(filename = "south_corplot.png",
       plot = south_cor_bargraph,
       device = "png",
       path = "3.Outputs/Models_ExploratoryPlots")

#        [Box-Whisker Plots]                                                ####

boxwhisk_data <- pivot_longer(deer_south,cols = starts_with("proportion"))

south_boxwhisk <- ggplot(boxwhisk_data, aes(x = name,y = value, fill = choice))+
  geom_boxplot() 

ggsave(filename = "south_boxwhisk.png",
       plot = south_boxwhisk,
       device = "png",
       path = "3.Outputs/Models_ExploratoryPlots",
       width = 10,
       height = 6,
       units = "in"
)


###############################################################################
#   [Models]                                                                ####
#      [North]                                                              ####

# Global Model
model_north_global <- glm(choice ~ proportion_1 + proportion_2 + proportion_3 + proportion_4 + 
                            proportion_5 + proportion_6 + proportion_8, 
                          data = deer_north, family = "binomial", na.action = "na.fail")

summary(model_north_global)

# Dredge Model
model_north_dredge <- dredge(model_north_global)

summary(model_north_dredge)

#      [South]                                                              ####

# Global Model
model_south_global <- glm(choice ~ proportion_1 + proportion_2 + proportion_3 + proportion_4 + 
                            proportion_5 + proportion_6 + proportion_8 + proportion_9 + proportion_10, 
                          data = deer_south, family = "binomial", na.action = "na.fail")

summary(model_south_global)

# Dredge Model
model_north_dredge <- dredge(model_south_global)

summary(model_north_dredge)
