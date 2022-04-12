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
library(pbapply)

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
#   [Covariate Extraction:North]                                            ####
#     [Making and registering cluster]                                      ####
cl <- makeCluster(2)
registerDoParallel(cl)
clusterEvalQ(cl, library("tidyverse"))

#       [Used]                                                              ####

# Establishing 420 meter buffers around points
extracts <- terra::extract(NLCD_north, deer_north_sf, buffer = 420)

clusterExport(cl, c("extracts"))


covariates_north_used <- pblapply(1:length(extracts),
                                  FUN = function(iterator){
                                    tryCatch(expr = {counts_x <- table(extracts[[iterator]])
                                    
                                    proportions_x <- prop.table(counts_x) %>% 
                                      as.data.frame() %>% 
                                      pivot_wider(names_from = Var1, values_from = Freq,names_prefix = "proportion_") %>% 
                                      add_column("choice" = 1,.before = 1) %>% 
                                      add_column("iterator" = iterator,.before = 1)
                                    
                                    return(proportions_x)},
                                    error=function(e) data.frame("iterator" = iterator))}, 
                                  cl = cl) %>% bind_rows()
# Stopping Cluster 
stopCluster(cl)

# Eliminating Rows that are all NA 
covariates_north_used <- covariates_north_used[rowSums(is.na(covariates_north_used)) != ncol(covariates_north_used)-1,]

#     [Making and registering cluster]                                      ####
cl <- makeCluster(2)
registerDoParallel(cl)
clusterEvalQ(cl, library("tidyverse"))

#       [Available]                                                         ####

# Creating available points
available_north <- st_sample(deer_north_mcp_sf,size = nrow(covariates_north_used)) %>% st_as_sf()

# Establishing 420 meter buffers around points
extracts <- terra::extract(NLCD_north, available_north, buffer = 420)

clusterExport(cl, c("extracts"))

covariates_north_available <- pblapply(1:length(extracts),
                                  FUN = function(iterator){
                                    tryCatch(expr = {counts_x <- table(extracts[[iterator]])
                                    
                                    proportions_x <- prop.table(counts_x) %>% 
                                      as.data.frame() %>% 
                                      pivot_wider(names_from = Var1, values_from = Freq,names_prefix = "proportion_") %>% 
                                      add_column("choice" = 0,.before = 1) %>% 
                                      add_column("iterator" = iterator,.before = 1)
                                    
                                    return(proportions_x)},
                                    error=function(e) data.frame("iterator" = iterator))}, 
                                  cl = cl) %>% bind_rows()

# Stopping Cluster 
stopCluster(cl)

#     [Joining Data]                                                        ####

north_final_data <- bind_rows(covariates_north_used,covariates_north_available)


###############################################################################
#      [Covariate Extraction:South]                                         ####

#        [Used]                                                             ####

# Establishing 420 meter buffers around points
extracts <- terra::extract(NLCD_south, deer_south_sf, buffer = 420)
length(extracts)

covariates_south_used <- foreach(i = 1:length(extracts),.combine = bind_rows) %dopar% {
  library(tidyverse)
  counts_x <- table(extracts[[i]])
  
  proportions_x <- prop.table(counts_x) %>% 
    as.data.frame() %>% 
    pivot_wider(names_from = Var1, values_from = Freq,names_prefix = "proportion_")} %>% 
  
  add_column("choice" = 1,
             .before = 1)

#        [Available]                                                        ####

# Creating available points
available_south <- st_sample(deer_south_mcp_sf,size = 10) %>% st_as_sf()

# Establishing 420 meter buffers around points
extracts <- terra::extract(NLCD_south, available_south, buffer = 420)

length(extracts)

covariates_south_available <- foreach(i = 1:length(extracts),.combine = bind_rows) %dopar% {
  library(tidyverse)
  counts_x <- table(extracts[[i]])
  
  proportions_x <- prop.table(counts_x) %>% 
    as.data.frame() %>% 
    pivot_wider(names_from = Var1, values_from = Freq,names_prefix = "proportion_")} %>% 
  
  add_column("choice" = 0,
             .before = 1)

#        [Joining Data]                                                     ####

south_final_data <- bind_rows(covariates_south_used,covariates_south_available)



#      [Closing back-end cluster]                                           ####

unregister()

###############################################################################
#   [Data Organization/Export]                                              ####
#      [Data Organization]                                                  ####
#        [North]                                                            ####

# Removing rows that are all NA 
north_final_clean <- north_final_data[rowSums(is.na(north_final_data)) != ncol(north_final_data)-1,]

# Making NAs 0 which indiciated zero of x resource
north_final_clean[is.na(north_final_clean)] <- 0

# Exporting Data
write_csv(x = north_final_clean,
          file = "3.Outputs/deer_north_final.csv")


#        [South]                                                            ####


