#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-04-09 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(ggplot2)
library(raster)
library(sf)
library(mapview)
library(adehabitatHR)
library(foreach)
library(doParallel)

#      Functions                                                            ####
source("D:/Drive/Research/UMontana/2.INPROGRESS/2.Clawson_RSF/2.R_Code/2.Functions/reclass_matrices.R")

unregister <- function() {
  
  # This function is to unregister the parallel backend of the doParallel and
  # foreach loop. Original source: https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  
}

#      Data                                                                 ####
#        [Deer Locations]                                                   ####
deer <- read_csv("1.Data/deer_all_clean.csv")

#        [Rasters]                                                          ####
NLCD <- raster("1.Data/NLCD_Missouri.tif") %>% ratify()

###############################################################################
#   [Study Population Boundaries]                                           ####
#      [North]                                                              ####

#        [Extracting North Population]                                      ####
deer_north <- deer %>% filter(site == "North")

#        [Making an MCP of the Population]                                  ####

# Creating SpatialPointsDataFrame
spdf_north <- deer_north
coordinates(spdf_north) <- ~x+y
proj4string(spdf_north) <- CRS("+init=epsg:5070")

# Creating MCP 
mcp_north <- mcp(spdf_north, percent=95, unin = c("m"),
    unout = c("km2"))

#        [Eliminating out-of-boundary points]                               ####
deer_north_sf <- st_as_sf(spdf_north) 

deer_north_mcp_sf <- st_as_sf(mcp_north)

sf_deer_north <- st_intersection(deer_north_sf,deer_north_mcp_sf)

#      [South]                                                              ####

#        [Extracting south Population]                                      ####
deer_south <- deer %>% filter(site == "South")

#        [Making an MCP of the Population]                                  ####

# Creating SpatialPointsDataFrame
spdf_south <- deer_south
coordinates(spdf_south) <- ~x+y
proj4string(spdf_south) <- CRS("+init=epsg:5070")

# Creating MCP 
mcp_south <- mcp(spdf_south, percent=95, unin = c("m"),
                 unout = c("km2"))

#        [Eliminating out-of-boundary points]                               ####
deer_south_sf <- st_as_sf(spdf_south) 

deer_south_mcp_sf <- st_as_sf(mcp_south)

sf_deer_south <- st_intersection(deer_south_sf,deer_south_mcp_sf)

###############################################################################
#   [Landcover Rasters]                                                     ####
#      [North]                                                              ####
#        [Cropping Raster]                                                  ####
NLCD_north <- crop(NLCD,sf_deer_north)

#        [Reclassifying Landcover Covariates]                               ####

NLCD_north <- NLCD_north %>% reclassify(reclass_matrixNorth) %>% ratify()


#      [South]                                                              ####
#        [Cropping Raster]                                                  ####
NLCD_south <- crop(NLCD,sf_deer_south)

#        [Reclassifying Landcover Covariates]                               ####

NLCD_south <- NLCD_south %>% reclassify(reclass_matrixSouth) %>% ratify()


###############################################################################
#   [Covariate Extraction]                                                  ####
#      [Making and registering cluster]                                     ####
cl <- makeCluster(5)
registerDoParallel(cl)

#      [North]                                                              ####
#        [Used]                                                             ####

# Establishing 420 meter buffers around points


extracts <- terra::extract(NLCD_north, deer_north_sf, buffer = 420)

landcover_proportions <- foreach(i = 1:length(extracts),.combine = bind_rows) %dopar% {
  
  counts_x <- table(extracts[[i]])
  
  proportions_x <- prop.table(counts_x) %>% 
    as.data.frame() %>% 
    pivot_wider(names_from = Var1, values_from = Freq,names_prefix = "proportion_")}


#        [Available]                                                        ####
#        [Joining Data]                                                     ####


available <- st_sample(deer_north_mcp_sf,
                       size = 10)


mapview(available)
#      [South]                                                              ####



#      [Closing back-end cluster]                                           ####

unregister()