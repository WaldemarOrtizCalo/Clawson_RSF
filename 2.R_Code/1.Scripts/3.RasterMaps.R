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

#      Functions                                                            ####
source("2.R_Code/2.Functions/reclass_matrices.R")

#      Data                                                                 ####

#        [Shapefiles]                                                       ####
Missouri_shp <- st_read("1.Data/RawData/shp_Missouri.shp")

#        [Rasters]                                                          ####
Missouri_NLCD <- raster("1.Data/RawData/NLCD_Missouri.tif")

#        [Regions]                                                          ####
Regions <- read_csv("1.Data/RawData/Regions_Clawson.csv")
counties_north <- filter(Regions, North_South == "North")
counties_south <- filter(Regions, North_South == "South")

###############################################################################
#   [Subsetting Shapefiles by Regions]                                      ####

North_shp <- subset(Missouri_shp, name_ucase %in% as.vector(counties_north$County))
South_shp <- subset(Missouri_shp, name_ucase %in% as.vector(counties_south$County))

###############################################################################
#   [Raster Prep]                                                           ####
#      [Cropping NLCD Rasters, Ratifying, and Reclassifying]                ####
North_NLCD <- crop(Missouri_NLCD,North_shp) %>% mask(North_shp) %>% ratify() %>% 
  reclassify(reclass_matrixNorth)

South_NLCD <- crop(Missouri_NLCD,South_shp) %>% mask(South_shp) %>% ratify() %>% 
  reclassify(reclass_matrixSouth)



###############################################################################
# DEV 
library(landscapemetrics)
test_raster <- raster(matrix(rep(c(1,2,2,1,3),10),ncol = 5,byrow = T))

plot(test_raster)


unique_cover <- unique(test_raster)

focal <- raster::focal(x = test_raster,
                       w = matrix(1,3,3),
                       fun = mean,
                       na.rm = T,
                       pad=T,padValue=0)


plot(focal)
as.matrix(focal)
as.matrix(test_raster)
