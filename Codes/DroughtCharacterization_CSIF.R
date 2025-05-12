# ===============================================================
# Setup Environment 
# ===============================================================
# Set the working directory and load necessary libraries
setwd("/Users/Qiang/Documents/R/Work/Drought/CSIF")
getwd()

# Load required packages
library(raster)
library(plyr)
library(rgdal)
library(zoo)
library(signal)
library(snow)
library(parallel)  # For multi-core processing

detectCores() # Display the number of available CPU cores

# ===============================================================
# Function Definitions
# ===============================================================

# Function to calculate baseline statistics: Max, Min, and Range
maxminfun <- function(ts_org) {
  max_val <- max(ts_org[1:92], na.rm = TRUE)
  min_val <- min(ts_org[1:92], na.rm = TRUE)
  range_val <- max_val - min_val
  return(c(max = max_val, min = min_val, range = range_val))
}

# Function to calculate the mean value during the growing season (DOY 39 to 61)
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
# Initialize a cluster with available cores minus 4 to avoid overload
beginCluster(detectCores() - 4)   

# Apply the maxminfun function to each raster layer in the stack
baseline_stats <- clusterR(sif_baseline_stack, calc, args = list(fun = maxminfun))

# Name the resulting raster layers
names(baseline_stats) <- c('max', 'min', 'range')

# Write the raster outputs to disk
writeRaster(baseline_stats, 
            filename = '/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline', 
            format = 'GTiff', datatype = 'FLT4S', overwrite = TRUE, 
            bylayer = TRUE, suffix = 'names')

# Close the cluster to free up resources
endCluster()

# ===============================================================
# Compute Growing Season Mean
# ===============================================================
beginCluster(detectCores() - 4)   

# Apply the meanfun function to calculate growing season mean for the baseline
growing_season_mean <- clusterR(sif_baseline_stack, calc, args = list(fun = meanfun))

# Name the output layer
names(growing_season_mean) <- 'growing'

# Write the growing season mean to disk
writeRaster(growing_season_mean, 
            filename = '/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline_growing', 
            format = 'GTiff', datatype = 'FLT4S', overwrite = TRUE, 
            bylayer = TRUE, suffix = 'names')

# Close the cluster
endCluster()

# ===============================================================
# Compute Relative Change (2015 drought year vs Baseline)
# ===============================================================
# Load the calculated rasters for baseline, 2015, and range
growing_2015 <- raster('/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_2015.tif')
growing_baseline <- raster('/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline.tif')
range_raster <- raster('/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_baseline_range.tif')

# Compute the relative change as a percentage
RelativeChange_before <- (growing_2015 - growing_baseline) / range_raster * 100

# Plot the relative change for visualization
plot(RelativeChange_before)

# Save the relative change raster to disk
writeRaster(RelativeChange_before, 
            filename = '/Users/Qiang/Documents/R/Work/Drought/CSIF/CSIF_RelativeChange', 
            format = 'GTiff', datatype = 'FLT4S', overwrite = TRUE, 
            bylayer = TRUE, suffix = 'names')