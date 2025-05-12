# Set working directory --------------------------------------------------------
setwd("..")
setwd("your path")
getwd()
 
# Load libraries ---------------------------------------------------------------
library(raster)
library(ggplot2)
library(dplyr)
library(rgdal)
library(sf)
library(rio)
library(tidyverse)

# Load and process SPI and CSIF data -------------------------------------------
# Load Meteorological Drought data (SPI index)
MeteorologicalDrought <- raster("./Results/Fig. 1c_SPI.tif")
MeteorologicalDrought

# Load Agricultural Productivity data (CSIF Relative Change)
AgriculturalProductivity <- raster("./Results/Fig. 1d_CSIF_RelativeChange.tif")
AgriculturalProductivity

# Resample Meteorological Drought raster to match Agricultural Productivity resolution
MeteorologicalDrought_resample <- resample(MeteorologicalDrought, AgriculturalProductivity, method = "bilinear")
MeteorologicalDrought_resample
AgriculturalProductivity

# Load boundary data for DPRK and ROK
DPRK <- st_read("./Data/Others/Boundry_DPRK.shp")
DPRK <- st_transform(DPRK, crs(MeteorologicalDrought))
DPRK_sp <- as(DPRK, "Spatial")  # Convert sf object to Spatial object

ROK <- st_read("./Data/Others/Boundry_ROK.shp")
ROK <- st_transform(ROK, crs(MeteorologicalDrought))
ROK_sp <- as(ROK, "Spatial") 

# Clip and mask the drought data for DPRK and ROK
# DPRK
MeteorologicalDrought_clipped_DPRK <- crop(MeteorologicalDrought_resample, DPRK_sp)
MeteorologicalDrought_masked_DPRK <- mask(MeteorologicalDrought_clipped_DPRK, DPRK_sp)
plot(MeteorologicalDrought_masked_DPRK)

AgriculturalProductivity_clipped_DPRK <- crop(AgriculturalProductivity, DPRK_sp)
AgriculturalProductivity_masked_DPRK <- mask(AgriculturalProductivity_clipped_DPRK, DPRK_sp)
plot(AgriculturalProductivity_masked_DPRK)

# ROK
MeteorologicalDrought_clipped_ROK <- crop(MeteorologicalDrought_resample, ROK_sp)
MeteorologicalDrought_masked_ROK <- mask(MeteorologicalDrought_clipped_ROK, ROK_sp)
plot(MeteorologicalDrought_masked_ROK)

AgriculturalProductivity_clipped_ROK <- crop(AgriculturalProductivity, ROK_sp)
AgriculturalProductivity_masked_ROK <- mask(AgriculturalProductivity_clipped_ROK, ROK_sp)
plot(AgriculturalProductivity_masked_ROK)

# Convert raster data to data frames -------------------------------------------
# DPRK
MeteorologicalDrought_df_DPRK <- as.data.frame(rasterToPoints(MeteorologicalDrought_masked_DPRK))
AgriculturalProductivity_df_DPRK <- as.data.frame(rasterToPoints(AgriculturalProductivity_masked_DPRK))

# Rename columns for merging
colnames(MeteorologicalDrought_df_DPRK) <- c("x", "y", "Mete_DPRK")
colnames(AgriculturalProductivity_df_DPRK) <- c("x", "y", "Agri_DPRK")

# Merge data frames by coordinates (x, y)
df_DPRK <- merge(MeteorologicalDrought_df_DPRK, AgriculturalProductivity_df_DPRK, by = c("x", "y"))
glimpse(df_DPRK)
write.csv(df_DPRK, file = "df_DPRK.csv")

# ROK
MeteorologicalDrought_df_ROK <- as.data.frame(rasterToPoints(MeteorologicalDrought_masked_ROK))
AgriculturalProductivity_df_ROK <- as.data.frame(rasterToPoints(AgriculturalProductivity_masked_ROK))

# Rename columns for merging
colnames(MeteorologicalDrought_df_ROK) <- c("x", "y", "Mete_ROK")
colnames(AgriculturalProductivity_df_ROK) <- c("x", "y", "Agri_ROK")

# Merge data frames by coordinates (x, y)
df_ROK <- merge(MeteorologicalDrought_df_ROK, AgriculturalProductivity_df_ROK, by = c("x", "y"))
glimpse(df_ROK)
write.csv(df_ROK, file = "df_ROK.csv")


# Note: the users should merge "df_DPRK.csv" and "df_ROK.csv" locally into a single CSV file: Data_DroughtCharacterization_SPI_CSIF_series.csv.
# You can also find this file in "./Data/Data_DroughtCharacterization_SPI_CSIF_series.csv"

# Load the merged data for analysis --------------------------------------------

df <- import("Data_DroughtCharacterization_SPI_CSIF_series.csv")

# Pivot to long format for analysis
df_Mete_long <- pivot_longer(df, cols = 1:2, names_to = "Country_Mete", values_to = "Mete")
df_Agri_long <- pivot_longer(df, cols = 3:4, names_to = "Country_Agri", values_to = "Agri")

# Combine data
df_long <- cbind(df_Mete_long, df_Agri_long) %>% 
  dplyr::select(Country_Mete, Mete, Agri)
glimpse(df_long)


# Correlation analysis ---------------------------------------------------------
# DPRK
correlation_DPRK <- cor(df_DPRK$Mete_DPRK, df_DPRK$Agri_DPRK, use = "complete.obs")
print(paste("Correlation coefficient (DPRK):", correlation_DPRK))

# ROK
correlation_ROK <- cor(df_ROK$Mete_ROK, df_ROK$Agri_ROK, use = "complete.obs")
print(paste("Correlation coefficient (ROK):", correlation_ROK))

# Visualization ----------------------------------------------------------------
p <- ggplot(df_long, aes(x = Mete, y = Agri, color = Country_Mete)) +
  geom_point(alpha = 0.1, shape = 16) +   
  geom_smooth(method = "lm", linewidth = 0.8) +  
  annotate("text", x = -0.5, y = -15, label = paste("r=0.31, p＜0.01"), size = 3, color = "#762a83") +  
  annotate("text", x = -1, y = 25, label = paste("r=0.13, p＜0.01"), size = 3, color = "#1b7837") +  
  xlab(bquote(SPI[TerraClimate])) + 
  ylab("CSIF Relative Change (%)") +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.text.y = element_text(size = 9, color = "black"),
        legend.title = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank()) +
  scale_color_manual(breaks = c("Mete_DPRK", "Mete_ROK"),
                     labels = c("DPRK", "ROK"),
                     values = c("#762a83", "#1b7837")) +
  scale_y_continuous(position = "right")
p

# Save the plot
ggsave(filename = "Fig.1f_ScatterPlot_spi_CSIF.png", width = 7.5, height = 5, units = "cm", dpi = 600)
