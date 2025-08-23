# Working directory
setwd("..")
setwd("your path")
getwd()

# Packages
library(tidyverse)
library(rio)
library(ggplot2)
library(ggplotify)
library(ggimage)
library(reshape2)
library(cowplot)
library(readxl)
library(ggpubr)
library(ggsci)
library(RColorBrewer)
library(tidyr)
library(dplyr)

#*******************************************************************************
# Rice yield ###################################################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data <- data %>% 
  select(year, DPRK_RiceYield, ROK_RiceYield)
data <- data[-c(1:15),]
glimpse(data)

# Calculate CV
dprk_sd <- sd(data$DPRK_RiceYield)
rok_sd <- sd(data$ROK_RiceYield)
dprk_mean <- mean(data$DPRK_RiceYield)
rok_mean <- mean(data$ROK_RiceYield)
dprk_cv <- dprk_sd / dprk_mean
rok_cv <- rok_sd / rok_mean

# long data
data_long <- pivot_longer(data, cols = 2:3, names_to = "rice", values_to = "value")
data_long$rice <- factor(data_long$rice, levels = c("DPRK_RiceYield", "ROK_RiceYield"),
                         labels = c("DPRK", "ROK"))

df_smoothed <- data_long %>%
  group_by(rice) %>%
  do(data.frame(year = seq(min(.$year), max(.$year), length.out = 10000),
                smooth_value = approx(.$year, .$value, xout = seq(min(.$year), max(.$year), length.out = 10000))$y))

# Visualization
p_rice <- ggplot(data_long, aes(year, value)) + 
  annotate("text", size = 2, x = 2011, y = 2.8, label = paste("CV =", round(dprk_cv, 2)), color = "#DE2D26") +
  annotate("text", size = 2, x = 2011, y = 7.8, label = paste("CV =", round(rok_cv, 2)), color = "#174994") +
  
  geom_line(aes(color = rice), size = 0.6, alpha = 0.9) +
  scale_color_manual(breaks = c("DPRK", "ROK"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0, 1), legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(2, "pt"),
        legend.key.width = unit(10, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm")),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text = element_text(size = 7, color = "black")
  ) +   
  
  xlab("Year") + 
  ylab(expression(atop("Rice yield", "(t/ha)"))) + 
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(1, 9), breaks = c(2, 4, 6, 8),
                     labels = c("2", "4", "6", "8"), expand = c(0, 0)
                     )

p_rice

#*******************************************************************************
# Land area equipped for irrigation #############################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_LandAreaEquippedForIrrigation, 
                             ROK_LandAreaEquippedForIrrigation)
glimpse(data_select)

# Long data
data_long_equipped <- pivot_longer(data_select, cols = 2:3, names_to = "equipped", values_to = "value_equipped")

# Visualization
p_equipped <- ggplot(data_long_equipped) + 
  geom_line(aes(x = year, y = value_equipped, color = equipped), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_LandAreaEquippedForIrrigation", "ROK_LandAreaEquippedForIrrigation"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(38, 60), breaks = c(40, 45, 50, 55),
                     labels = c("40", "45", "50", "55"), expand = c(0, 0)) +
  
  ylab(expression(atop("Cropland equipped", "for irrigation (%)"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(2, "pt"),
        legend.key.width = unit(12, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2)
  )

p_equipped

#*******************************************************************************
# Total dam capacity ###########################################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_TotalDamCapacity = NK_TotalDamCapacity, 
                             ROK_TotalDamCapacity = SK_TotalDamCapacity)
glimpse(data_select)

# Long data
data_long_dam <- pivot_longer(data_select, cols = 2:3, names_to = "dam", values_to = "value_dam")

# Visualization
p_dam <- ggplot(data_long_dam) + 
  geom_line(aes(x = year, y = value_dam, color = dam), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_TotalDamCapacity", "ROK_TotalDamCapacity"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(7, 22), breaks = c(12, 16, 20),
                     labels = c("12", "16", "20"), expand = c(0, 0)) +
  
  ylab(expression(atop("Total dam capacity", "(km"^3*")"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(2, "pt"),
        legend.key.width = unit(12, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_dam

#*******************************************************************************
# Irrigated agriculture water use efficiency ###################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_IrrigatedAgricultureWaterUseEfficiency, 
                             ROK_IrrigatedAgricultureWaterUseEfficiency)
glimpse(data_select)

# Long data
data_long_wue <- pivot_longer(data_select, cols = 2:3, names_to = "wue", values_to = "value_wue")

# Visualization
p_wue <- ggplot(data_long_wue) + 
  geom_line(aes(x = year, y = value_wue, color = wue), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_IrrigatedAgricultureWaterUseEfficiency", "ROK_IrrigatedAgricultureWaterUseEfficiency"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(0.3, 1.7), breaks = c(0.6, 1.0, 1.4),
                     labels = c("0.6", "1.0", "1.4"), expand = c(0, 0)) +
  
  ylab(expression(atop("Irrigation WUE", "(dollars/m"^3*")"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(2, "pt"),
        legend.key.width = unit(12, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_wue

#*******************************************************************************
# Water consumption coefficient ################################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_WaterConsumptionCoefficient, 
                             ROK_WaterConsumptionCoefficient)
glimpse(data_select)

# Long data
data_long_wcc <- pivot_longer(data_select, cols = 2:3, names_to = "wcc", values_to = "value_wcc")

# Visualization
p_wcc <- ggplot(data_long_wcc) + 
  geom_line(aes(x = year, y = value_wcc, color = wcc), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_WaterConsumptionCoefficient", "ROK_WaterConsumptionCoefficient"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(0.2, 1.65), breaks = c(0.6, 1.0, 1.4),
                     labels = c("0.6", "1.0", "1.4"), expand = c(0, 0)) +
  
  ylab(expression(atop("Water consumption", "coefficient"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(2, "pt"),
        legend.key.width = unit(12, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_wcc

#*******************************************************************************
# Electricity generation #######################################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_ElectricityGeneration, 
                             ROK_ElectricityGeneration)
glimpse(data_select)

# Long data
data_long <- pivot_longer(data_select, cols = 2:3, names_to = "elec", values_to = "value")

# Visualization
p_electricity <- ggplot(data_long) + 
  geom_line(aes(x = year, y = value, color = elec), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_ElectricityGeneration", "ROK_ElectricityGeneration"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(0, 6), breaks = c(1, 3, 5),
                     labels = c("1", "3", "5"), expand = c(0, 0)) +
  
  ylab(expression(atop("Electricity generation", "(10"^11*"kWh)"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_electricity

#*******************************************************************************
# Coal production ##############################################################
#*******************************************************************************

data <- read_excel("Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_CoalProduction, 
                             ROK_CoalProduction)
glimpse(data_select)

# Long data
data_long <- pivot_longer(data_select, cols = 2:3, names_to = "coalprod", values_to = "value")

# Visualization
p_coalprod <- ggplot(data_long) + 
  geom_line(aes(x = year, y = value, color = coalprod), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_CoalProduction", "ROK_CoalProduction"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(0, 4.5), breaks = c(1, 2, 3, 4),
                     labels = c("1", "2", "3", "4"), expand = c(0, 0)) +
  
  ylab(expression(atop("Coal production", "(10"^4*"Mst)"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_coalprod


#*******************************************************************************
# Crude oil import #############################################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_CrudeOilImport, 
                             ROK_CrudeOilImport)
glimpse(data_select)

# Long data
data_long <- pivot_longer(data_select, cols = 2:3, names_to = "oil", values_to = "value")

# Visualization
p_oil <- ggplot(data_long) + 
  geom_line(aes(x = year, y = value, color = oil), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_CrudeOilImport", "ROK_CrudeOilImport"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(-0.1, 3.1), breaks = c(1, 2, 3),
                     labels = c("1", "2", "3"), expand = c(0, 0)) +
  
  ylab(expression(atop("Crude oil import", "(10"^3*"Mb/d)"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_oil


#*******************************************************************************
# Coal import ##################################################################
#*******************************************************************************

data <- read_excel("./Data/Data_Statistics.xlsx")
glimpse(data)

data_select <- dplyr::select(data, year, 
                             DPRK_CoalImport, 
                             ROK_CoalImport)
glimpse(data_select)

# Long data
data_long <- pivot_longer(data_select, cols = 2:3, names_to = "coal", values_to = "value")

# Visualization
p_coal <- ggplot(data_long) + 
  geom_line(aes(x = year, y = value, color = coal), size = 0.6, alpha = 0.9) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("DPRK_CoalImport", "ROK_CoalImport"),
                     labels = c("DPRK", "ROK"), 
                     values = c("#DE2D26", "#174994")) +
  
  scale_x_continuous(limits = c(1980, 2020), breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c("1980", "1990", "2000", "2010", "2020")) +
  scale_y_continuous(limits = c(-0.1, 1.7), breaks = c(0.5, 1.0, 1.5),
                     labels = c("0.5", "1.0", "1.5"), expand = c(0, 0)) +
  
  ylab(expression(atop("Coal import", "(10"^5*"Mst)"))) + 
  theme_bw() +
  
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", size = 0.2),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_coal


#*******************************************************************************
# Combined plot ################################################################
#*******************************************************************************

library("Rmisc")
library("ggpubr")
library("patchwork")

plot_grid(p_rice, p_equipped, p_dam, p_wue, p_wcc, p_electricity, p_coalprod, p_oil, p_coal, 
          ncol = 3, 
          align = "v")

# Save 
ggsave(filename="Fig.2b-j.png",height=9, width=15, units="cm",dpi = 600)




