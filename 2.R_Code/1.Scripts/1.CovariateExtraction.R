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

#      Functions                                                            ####
source("D:/Drive/Research/UMontana/2.INPROGRESS/2.Clawson_RSF/2.R_Code/2.Functions/reclass_matrices.R")

#      Data                                                                 ####
#        [Deer Locations]                                                   ####
deer <- read_csv("1.Data/deer_all_clean.csv")

#        [Rasters]                                                          ####
NLCD <- raster("1.Data/NLCD_Missouri.tif") %>% ratify()

###############################################################################
#   [Extracting Study Population Boundaries]                                ####
#      [North]                                                              ####
#      [South]                                                              ####
###############################################################################
#   [Preparing Landcover Rasters]                                           ####

#   [Reclassifying Landcover Covariates]                                              ####

Missouri_NLCD_South <- Missouri_NLCD %>% reclassify(reclass_matrixSouth)

