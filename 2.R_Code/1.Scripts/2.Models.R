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
deer_north <- read_csv("1.Data/CleanData/deer_north_final.csv") %>% 
  rename(proportion_water = proportion_1,
         proportion_developed = proportion_2,
         proportion_barren = proportion_3,
         proportion_forest = proportion_4,
         proportion_shrub = proportion_5,
         proportion_grassland = proportion_6,
         proportion_cropland = proportion_7,
         proportion_wetland = proportion_8)

deer_south <- read_csv("1.Data/CleanData/deer_south_final.csv") %>% 
  rename(proportion_water = proportion_1,
         proportion_developed = proportion_2,
         proportion_barren = proportion_3,
         proportion_decidousforest = proportion_4,
         proportion_evergreenforest = proportion_5,
         proportion_mixedforest = proportion_6,
         proportion_shrub = proportion_7,
         proportion_grassland = proportion_8,
         proportion_cropland = proportion_9,
         proportion_wetland = proportion_10)


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
  geom_boxplot() +
  labs(x = "covariates")

ggsave(filename = "north_boxwhisk.png",
       plot = north_boxwhisk,
       device = "png",
       path = "3.Outputs/Models_ExploratoryPlots",
       width = 14,
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
  geom_boxplot() +
  labs(x = "covariates")

ggsave(filename = "south_boxwhisk.png",
       plot = south_boxwhisk,
       device = "png",
       path = "3.Outputs/Models_ExploratoryPlots",
       width = 16,
       height = 6,
       units = "in"
)


###############################################################################
#   [Models]                                                                ####
#      [North]                                                              ####

# Global Model
model_north_global <- glm(choice ~ proportion_water + proportion_developed +
                             proportion_barren + proportion_forest + proportion_shrub + 
                             proportion_grassland +  proportion_wetland,
                          data = deer_north, family = binomial(link='logit'), na.action = "na.fail")


summary(model_north_global)


# Dredge Model
model_north_dredge <- dredge(model_north_global)

summary(model_north_dredge)

# Saving Dredge Output
write_csv(x = model_north_dredge,
          file = "3.Outputs/ModelOutputs/NorthDredge.csv")

#      [South]                                                              ####

# Global Model
model_south_global <- glm(choice ~ proportion_water +
                            proportion_developed +
                            proportion_barren +
                            proportion_decidousforest +
                            proportion_evergreenforest +
                            proportion_mixedforest +
                            proportion_shrub +
                            proportion_grassland +
                            proportion_wetland, 
                          data = deer_south, family = binomial(link='logit'), na.action = "na.fail")



summary(model_south_global)

# Dredge Model
model_south_dredge <- dredge(model_south_global)

summary(model_south_dredge)

# Saving Dredge Output
write_csv(x = model_south_dredge,
          file = "3.Outputs/ModelOutputs/SouthDredge.csv")

#      [Model Exports]                                                      ####

# North
saveRDS(model_north_global,
        file = "3.Outputs/ModelOutputs/GlobalModelNorth")

# South
saveRDS(model_south_global,
        file = "3.Outputs/ModelOutputs/GlobalModelSouth")

###############################################################################