# ===============================================================
# Setup Environment 
# ===============================================================

# Working space
setwd("/Users/Qiang/Documents/R/Work/Drought/CSIF")
getwd()

# Packages
library(raster)
library(plyr)
library(rgdal)
library(zoo)
library(signal)
library(snow)
library(parallel) 

detectCores() 

# ===============================================================
# Functions
# ===============================================================

# Function to calculate baseline Max, Min, and Range
maxminfun <- function(ts_org) {
  max_val <- max(ts_org[1:92], na.rm = TRUE)
  min_val <- min(ts_org[1:92], na.rm = TRUE)
  range_val <- max_val - min_val
  return(c(max = max_val, min = min_val, range = range_val))
}

# Function to calculate the mean value during the growing season
meanfun <- function(ts_org) {
  growing_season_mean <- mean(ts_org[39:61], na.rm = TRUE)
  return(c(growing = growing_season_mean))
}

# ===============================================================
# Data Import and Stacking
# ===============================================================

# List all GeoTIFF files for baseline and 2015 SIF datasets
sif_baseline <- list.files("/Users/Qiang/Documents/R/Work/Drought/CSIF/Baseline_Anomaly_Masked_riceall_baseline", 
                           pattern = "*.tif$", full.names = TRUE, recursive = TRUE)
sif_2015 <- list.files("/Users/Qiang/Documents/R/Work/Drought/CSIF/Baseline_Anomaly_Masked_riceall_2015", 
                       pattern = "*.tif$", full.names = TRUE, recursive = TRUE)

# Stack the raster layers for batch processing
sif_baseline_stack <- raster::stack(sif_baseline)
sif_2015_stack <- raster::stack(sif_2015)

# ===============================================================
# Compute Baseline Statistics
# ===============================================================

beginCluster(detectCores() - 4)   

# Apply the maxminfun function
baseline_stats <- clusterR(sif_baseline_stack, calc, args = list(fun = maxminfun))
names(baseline_stats) <- c('max', 'min', 'range')

# Save
writeRaster(baseline_stats, 
            filename = '/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline', 
            format = 'GTiff', datatype = 'FLT4S', overwrite = TRUE, 
            bylayer = TRUE, suffix = 'names')

endCluster()

# ===============================================================
# Compute Growing Season Mean
# ===============================================================

beginCluster(detectCores() - 4)   

# Apply the meanfun function to calculate growing season mean for the baseline
growing_season_mean <- clusterR(sif_baseline_stack, calc, args = list(fun = meanfun))
names(growing_season_mean) <- 'growing'

# Save 
writeRaster(growing_season_mean, 
            filename = '/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline_growing', 
            format = 'GTiff', datatype = 'FLT4S', overwrite = TRUE, 
            bylayer = TRUE, suffix = 'names')

endCluster()

# ===============================================================
# Compute Relative Change (2015 drought year vs Baseline)
# ===============================================================

# Load the calculated rasters for baseline, 2015, and range
growing_2015 <- raster('/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_2015.tif')
growing_baseline <- raster('/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline.tif')
range_raster <- raster('/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline_range.tif')

# Compute the relative change
RelativeChange_before <- (growing_2015 - growing_baseline) / range_raster * 100
plot(RelativeChange_before)

# Save
writeRaster(RelativeChange_before, 
            filename = '/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_RelativeChange', 
            format = 'GTiff', datatype = 'FLT4S', overwrite = TRUE, 
            bylayer = TRUE, suffix = 'names')
