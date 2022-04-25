# Clawson_RSF

## Folder Structure

**1. Data**

All data associated with this analysis will be found here. Whether it be raw or clean data.

**2. R_Code**

Scripts,Functions, or Rmarkdown documents are stored here.

**3. Outputs**

Results, figures, and predictive rasters are stored here.

------------------------------------------------------------------------

## Analytical Considerations

-   I employed a logistic regression to estimate resource selection.

-   Based data exploration, Cropland and Forest cover had a negative correlation. Therefore, I dropped cropland covariates from all analyses and included forest cover covariates. Correlations values can be seen for the Northern study area (3.Outputs/Models_ExploratoryPlots/north_corplot.png) and Southern study area (3.Outputs/Models_ExploratoryPlots/south_corplot.png) and

------------------------------------------------------------------------

## Important Files

**3. Outputs/ModelOutputs/GlobalModelNorth**

This is the model object for the Northern study area model. The global model was the highest ranking one (AIC values can be checked in the NorthDredge.csv file).

**3. Outputs/ModelOutputs/GlobalModelSouth**

This is the model object for the Southern study area model. The global model was the highest ranking one (AIC values can be checked in the SouthDredge.csv file).

**3. Outputs/PredictiveRasters/North_PredictiveRaster.tif**

This is the predictive raster map for the Northern study area. This was calculated using coefficient values from the top Northern model (GlobalModelNorth).

**3. Outputs/PredictiveRasters/South_PredictiveRaster.tif**

This is the predictive raster map for the Southern study area. This was calculated using coefficient values from the top Southern model (GlobalModelSouth).

------------------------------------------------------------------------
