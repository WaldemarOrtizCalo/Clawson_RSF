#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-04-15 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(raster)
library(tidyverse)
library(sf)
library(mapview)
library(pbapply)
library(doParallel)

#      Functions                                                            ####
source("2.R_Code/2.Functions/reclass_matrices.R")
raster_prop_creation <- function(raster,landcover_num, export = F, export.filepath = NULL){ 
  
  if(export == F){
  r <- raster == landcover_num 
  r <- r %>% ratify
  
  fw <- ceiling(focalWeight(r, 420, type='circle'))
  
  r_focal <- raster::focal(x = r,
                           w = fw,
                           fun = mean,
                           na.rm = T,
                           pad=F)
  
  names(r_focal)<-paste0("proportion_",landcover_num)
  
  return(r_focal)
  }
  if(export == T){
    r <- raster == landcover_num 
    r <- r %>% ratify
    
    fw <- ceiling(focalWeight(r, 420, type='circle'))
    
    r_focal <- raster::focal(x = r,
                             w = fw,
                             fun = mean,
                             na.rm = T,
                             pad=F)
    
    names(r_focal)<-paste0("proportion_",landcover_num)
    
    writeRaster(r_focal,filename= paste0(export.filepath,names(r_focal)),format = "GTiff",overwrite = T)
    
    return(r_focal)
  }
}

#      Data                                                                 ####
#        [Shapefiles]                                                       ####
Missouri_shp <- st_read("1.Data/RawData/shp_Missouri.shp")

#        [Rasters]                                                          ####
Missouri_NLCD <- raster("1.Data/RawData/NLCD_Missouri.tif")

#        [Regions]                                                          ####
Regions <- read_csv("1.Data/RawData/Regions_Clawson.csv")
counties_north <- filter(Regions, North_South == "North")
counties_south <- filter(Regions, North_South == "South")

#        [Models]                                                           ####

# North
North_model <- readRDS(file = "3.Outputs/ModelOutputs/GlobalModelNorth")

# South
South_model <- readRDS(file = "3.Outputs/ModelOutputs/GlobalModelSouth")
###############################################################################
#   [Subsetting Shapefiles by Regions]                                      ####

North_shp <- subset(Missouri_shp, name_ucase %in% as.vector(counties_north$County))
South_shp <- subset(Missouri_shp, name_ucase %in% as.vector(counties_south$County))

###############################################################################
#   [Raster Prep]                                                           ####

#      [Cropping NLCD Rasters, Ratifying, and Reclassifying]                ####

North_NLCD <- crop(Missouri_NLCD,North_shp) %>% 
  mask(North_shp) %>% 
  ratify() %>% 
  reclassify(reclass_matrixNorth)

South_NLCD <- crop(Missouri_NLCD,South_shp) %>% 
  mask(South_shp) %>% 
  ratify() %>% 
  reclassify(reclass_matrixSouth)

###############################################################################
#   [Creating Proportion Rasters]                                           ####
#      [North]                                                              ####

# Registering Clusters 
cl <- makeCluster(3)
registerDoParallel(cl)
clusterEvalQ(cl, 
             {library(raster)
               library(tidyverse)})

# Calculating Proportions
North_raster_list <-pblapply(X = unique(North_NLCD), 
                             FUN = raster_prop_creation, raster = North_NLCD,export = T,
                             export.filepath = "1.Data/CleanData/North_",
                             cl = cl)

North_stack <- stack(North_raster_list)

# Stopping Cluster 
stopCluster(cl)

#      [South]                                                              ####

# Registering Clusters 
cl <- makeCluster(2)
registerDoParallel(cl)
clusterEvalQ(cl, 
             {library(raster)
               library(tidyverse)})

# Calculating Proportions
South_raster_list <-pblapply(X = unique(South_NLCD), 
                             FUN = raster_prop_creation, raster = South_NLCD,export = T,
                             export.filepath = "1.Data/CleanData/South_",
                             cl = cl)

South_stack <- stack(South_raster_list)

# Stopping Cluster 
stopCluster(cl)

###############################################################################
#   [Making Predictive Rasters]                                             ####
#      [Loading Rasters]                                                    ####

# North
North_prop_rasters <- lapply(list.files("1.Data/CleanData",
                                        pattern = "North_proportion",
                                        full.names = T), raster) %>% 
  stack() %>% 
  dropLayer(i = 7) # Eliminating Cropland since it was highly correlated

# South 
South_prop_rasters <- lapply(list.files("1.Data/CleanData",
                                        pattern = "South_proportion",
                                        full.names = T), raster) %>% 
  stack() %>% 
  dropLayer(i = 10) # Eliminating Cropland since it was highly correlated


#      [Loading Predictive Models]                                          ####

# North
North_model <- readRDS(file = "3.Outputs/ModelOutputs/GlobalModelNorth")

# South
South_model <- readRDS(file = "3.Outputs/ModelOutputs/GlobalModelSouth")


#   [Creating Rasters]                                                      ####

#      [Assigning raster names]                                             ####
names(North_prop_rasters) <- c("proportion_water",
                               "proportion_developed",
                               "proportion_barren",
                               "proportion_forest",
                               "proportion_shrub",
                               "proportion_grassland",
                               "proportion_wetland")

names(South_prop_rasters) <- c("proportion_water",
                               "proportion_wetland",
                               "proportion_developed",
                               "proportion_barren",
                               "proportion_decidousforest",
                               "proportion_evergreenforest",
                               "proportion_mixedforest",
                               "proportion_shrub",
                               "proportion_grassland"
                               )



#      [Making Predictive Rasters]                                          ####

#        [North]                                                            ####

North_model_coefs <- North_model$coefficients

predictive_raster_north <- exp(North_model_coefs[1] + 
                                 North_model_coefs[2]*North_prop_rasters[[1]]+
                                 North_model_coefs[3]*North_prop_rasters[[2]]+
                                 North_model_coefs[4]*North_prop_rasters[[3]]+
                                 North_model_coefs[5]*North_prop_rasters[[4]]+
                                 North_model_coefs[6]*North_prop_rasters[[5]]+
                                 North_model_coefs[7]*North_prop_rasters[[6]]+
                                 North_model_coefs[8]*North_prop_rasters[[7]]) / 
  (1 + exp(North_model_coefs[1] + 
            North_model_coefs[2]*North_prop_rasters[[1]]+
            North_model_coefs[3]*North_prop_rasters[[2]]+
            North_model_coefs[4]*North_prop_rasters[[3]]+
            North_model_coefs[5]*North_prop_rasters[[4]]+
            North_model_coefs[6]*North_prop_rasters[[5]]+
            North_model_coefs[7]*North_prop_rasters[[6]]+
            North_model_coefs[8]*North_prop_rasters[[7]]))



#        [South]                                                            ####

South_model_coefs <- South_model$coefficients

predictive_raster_south <- exp(South_model_coefs[1] + 
                                 South_model_coefs[2]*South_prop_rasters[[1]]+
                                 South_model_coefs[3]*South_prop_rasters[[3]]+
                                 South_model_coefs[4]*South_prop_rasters[[4]]+
                                 South_model_coefs[5]*South_prop_rasters[[5]]+
                                 South_model_coefs[6]*South_prop_rasters[[6]]+
                                 South_model_coefs[7]*South_prop_rasters[[7]]+
                                 South_model_coefs[8]*South_prop_rasters[[8]]+
                                 South_model_coefs[9]*South_prop_rasters[[9]]+
                                 South_model_coefs[10]*South_prop_rasters[[2]]) / 
  (1 +exp(South_model_coefs[1] + 
            South_model_coefs[2]*South_prop_rasters[[1]]+
            South_model_coefs[3]*South_prop_rasters[[3]]+
            South_model_coefs[4]*South_prop_rasters[[4]]+
            South_model_coefs[5]*South_prop_rasters[[5]]+
            South_model_coefs[6]*South_prop_rasters[[6]]+
            South_model_coefs[7]*South_prop_rasters[[7]]+
            South_model_coefs[8]*South_prop_rasters[[8]]+
            South_model_coefs[9]*South_prop_rasters[[9]]+
            South_model_coefs[10]*South_prop_rasters[[2]]))

#      [Exporting]                                                          ####

# North 
writeRaster(predictive_raster_north,
            filename = "3.Outputs/PredictiveRasters/North_PredictiveRasters.tif")

writeRaster(predictive_raster_south,
            filename = "3.Outputs/PredictiveRasters/South_PredictiveRasters.tif")
# South

#      [Checking Predictive Maps]                                           ####

# North 
mapview(predictive_raster_north)

# South
mapview(predictive_raster_south)

###############################################################################