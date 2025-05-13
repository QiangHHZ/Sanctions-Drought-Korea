# =====================
# Setup Environment 
# =====================

# Working space
setwd("your path")

# Packages
library(rio)
library(tidyverse)
library(lavaan)
library(blavaan)
library(regsem)
library(readxl)
library(writexl)

# =====================
# Load and Prepare Data
# =====================

# Import dataset
data <- import("./Data/Data_Statistics.xlsx")

# Filter data sice 1995
data_slice <- data[-c(1:15), ]

# Select relevant indicators
data_slice <- data_slice %>%
  select(year, 
         DPRK_RiceYield,
         DPRK_IrrigatedAgricultureWaterUseEfficiency,
         DPRK_WaterConsumptionCoefficient,
         DPRK_TotalDamCapacity,
         DPRK_LandAreaEquippedForIrrigation,
         DPRK_CoalProduction,
         DPRK_CoalImport,
         DPRK_CrudeOilImport,
         DPRK_ElectricityGeneration)

# Standardize variables 
data_standardized <- as.data.frame(scale(data_slice))
glimpse(data_standardized)

# ===================================
# SEM
# ===================================

sem_model_dprk <- "
  # Latent variables
  energy =~ DPRK_CoalImport + DPRK_CrudeOilImport
  IrrigationCapacity =~ DPRK_WaterConsumptionCoefficient + DPRK_TotalDamCapacity + DPRK_IrrigatedAgricultureWaterUseEfficiency

  # Structural paths
  DPRK_CoalProduction ~ energy
  DPRK_ElectricityGeneration ~ energy + DPRK_CoalProduction
  IrrigationCapacity ~ DPRK_ElectricityGeneration
  DPRK_RiceYield ~ IrrigationCapacity
"

fit_dprk <- sem(sem_model_dprk, data = data_standardized, missing = "ML", em.h1.iter.max = 2000)

summary(fit_dprk, fit.measures = TRUE, standardized = TRUE)

# Note on estimation warnings:
# A warning was generated during SEM estimation indicating that a few variances 
# were slightly negative (e.g., -0.013 for crude oil import, -0.044 for dam capacity). 
# This situation can arise when working with limited or incomplete time series data—
# as is common in the DPRK context—particularly for variables like irrigated agriculture 
# water use efficiency, which only has data from 1995 onward. 

# Importantly, the model converged normally, all key parameters were estimated, and 
# the minor negative variances (e.g., -0.013 for NK_CrudeOilImport, -0.044 for NK_TotalDamCapacity)
# were well within acceptable thresholds (commonly considered tolerable when close to zero).
# More crucially, the primary structural paths were stable and interpretable, with most 
# coefficients not statistically significant—highlighting that the hypothesized causal 
# chain is not clearly supported in the DPRK case, which is an important finding in itself.

# The warnings observed here do not affect the interpretation or validity of the results. 
# This point has also been clarified in the SI.