# Note: Due to the large file size (~21 GB), input data are not included in the repository.
# Users need to download the CSIF datasets manually at https://figshare.com/articles/CSIF/6387494
# And then save them in the "./Data" folder

# ==================================
# Setup Environment 
# =================================

# Working space
setwd("your path")
getwd()

# Packages
library(raster)
library(plyr)
library(rgdal)
library(zoo)
library(signal)
library(snow)
library(parallel) 
library(ncdf4)

detectCores() 

# ===================================
# Define Paths
# ================================

input_dir <- "./Data"
output_dir <- "./Data_tif"
baseline_years <- c(2003, 2004, 2006, 2010, 2012, 2013)
drought_year <- 2015
baseline_output_dir <- file.path(output_dir, "Baseline")
mask_output_dir <- file.path(output_dir, "Masked")

dir.create(baseline_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(mask_output_dir, showWarnings = FALSE, recursive = TRUE)

# ================================
# Convert NC to TIF
# ================================

convert_nc_to_tif <- function(years) {
  for (year in years) {
    year_path <- file.path(input_dir, as.character(year))
    nc_files <- list.files(year_path, pattern = "*.nc$", full.names = TRUE, recursive = TRUE)
    
    raster_stack <- raster::stack(nc_files, varname = "clear_inst_SIF")
    
    out_path <- file.path(output_dir, as.character(year))
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
    
    for (i in seq_along(nc_files)) {
      out_file <- file.path(out_path, paste0("OCO2.SIF.clear.inst.", year, sprintf("%03d", i), ".v2.tif"))
      writeRaster(raster_stack[[i]], filename = out_file, format = "GTiff")
      print(paste("Saved:", out_file))
    }
  }
}

convert_nc_to_tif(c(baseline_years, drought_year))

# ================================
# Calculate Baseline
# ================================

# Generate 4-day interval date strings
doy <- paste0(c(rep('00',3),rep('0',22),rep('',67)), c(seq(1,9,4),seq(13,97,4),seq(101,365,4)))

# Calculate the baseline average across multiple years: 2003, 2004, 2006, 2010, 2012, 2013
for (i in doy) {
  rasters <- lapply(baseline_years, function(year) {
    raster(file.path(output_dir, as.character(year), paste0("OCO2.SIF.clear.inst.", year, i, ".v2.tif")))
  })
  
  imgMean030406101213 <- Reduce(`+`, rasters) / length(rasters)
  
  out_file <- file.path(baseline_output_dir, paste0("Baseline030406101213_", i, ".tif"))
  writeRaster(imgMean030406101213, 
              filename = out_file, 
              format = 'GTiff', datatype = 'FLT4S', 
              overwrite = TRUE, bylayer = TRUE, suffix = 'names')
  
  print(paste("Baseline saved:", out_file))
}

# ================================
# Crop and Mask
# ================================

# Load Korea shp and risk raster for masking
korea <- read_sf('./Data/Others/Boundry_Korea.shp')
rice_croped <- raster('./Data/Others/Rice_ResampleCSIF_korea.tif')

# Loop for cropping and masking
for (i in doy) {
  baseline_file <- file.path(baseline_output_dir, paste0("Baseline030406101213_", i, ".tif"))
  
  if (file.exists(baseline_file)) {
    img <- raster(baseline_file)
    
    img_croped <- crop(img, rice_croped)
    extent(rice_croped) <- extent(img_croped)  
    img_masked <- mask(img_croped, rice_croped)
    
    out_file <- file.path(mask_output_dir, paste0("Masked_Baseline030406101213_", i, ".tif"))
    writeRaster(img_masked, 
                filename = out_file, 
                format = 'GTiff', datatype = 'FLT4S', 
                overwrite = TRUE, bylayer = TRUE, suffix = 'names')
    print(paste("Masked raster saved:", out_file))
  } else {
    print(paste("File not found:", baseline_file))
  }
}

# ================================================
# Calculate Max, Min, and Range of baseline
# =============================================

maxminfun <- function(ts_org) {
  max_val <- max(ts_org[1:92], na.rm = TRUE)
  min_val <- min(ts_org[1:92], na.rm = TRUE)
  range_val <- max_val - min_val
  return(c(max_val, min_val, range_val))
}

beginCluster(detectCores() - 4)   
result <- clusterR(stack(list.files(mask_output_dir, full.names = TRUE)), 
                   calc, args = list(fun = maxminfun))
names(result) <- c('max', 'min', 'range')

writeRaster(result, 
            filename = file.path(output_dir, "CSIF_baseline"), 
            format = 'GTiff', datatype = 'FLT4S', 
            overwrite = TRUE, bylayer = TRUE, suffix = 'names')
endCluster()

# ================================
# Calculate Growing Season Mean
# ================================

meanfun <- function(ts_org) {
  growing <- mean(ts_org[39:61], na.rm = TRUE)
  return(c(growing))
}

beginCluster(detectCores() - 4)
baseline_growing <- clusterR(stack(list.files(mask_output_dir, full.names = TRUE)), 
                             calc, args = list(fun = meanfun))
writeRaster(baseline_growing, 
            filename = file.path(output_dir, "CSIF_baseline_growing.tif"), 
            format = 'GTiff', datatype = 'FLT4S', 
            overwrite = TRUE)
endCluster()

growing_2015 <- raster(file.path(output_dir, "CSIF_2015_growing.tif"))
growing_baseline <- raster(file.path(output_dir, "CSIF_baseline_growing.tif"))
range_raster <- raster(file.path(output_dir, "CSIF_baseline_range.tif"))

# ================================
# Calculate Relative Change
# ================================

RelativeChange_before <- (growing_2015 - growing_baseline) / range_raster * 100
plot(RelativeChange_before)

writeRaster(RelativeChange_before, 
            filename = file.path(output_dir, "CSIF_RelativeChange.tif"), 
            format = 'GTiff', datatype = 'FLT4S', 
            overwrite = TRUE)
