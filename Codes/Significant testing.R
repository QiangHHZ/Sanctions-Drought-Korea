# Set working directory ########################################################
setwd("..")
setwd("/Users/Qiang/Documents/R/Work/Drought")
getwd()

# set the packages #############################################################
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
library(ggplot2)
library(ggsignif)

# Import data ####################################################################
data <- read_excel("NK_SK_Fig2_data.xlsx")  # Import data from the Excel file
glimpse(data)  # Preview the structure of the data

# Data selection and preparation ################################################
# Select relevant columns from the data
data_select <- dplyr::select(data, year, NK_LandAreaEquippedForIrrigation, SK_LandAreaEquippedForIrrigation)
data_select_slice <- data_select[-c(1:11),]  

# Separate data into two periods: Before and After 2006
before_2006 <- data_select_slice$NK_LandAreaEquippedForIrrigation[data_select_slice$year < 2006]
after_2006 <- data_select_slice$NK_LandAreaEquippedForIrrigation[data_select_slice$year >= 2006]

# Mann-Whitney U Test ###########################################################
wilcox_test_result <- wilcox.test(before_2006, after_2006)
print(wilcox_test_result)  # Print the test result

# Prepare data for plotting ######################################################
# Create a data frame for the boxplot
df <- data.frame(
  year = rep(c("Before 2006", "After 2006"), c(length(before_2006), length(after_2006))),
  efficiency = c(before_2006, after_2006)
)

# Ensure the 'year' column is a factor with the correct order
df$year <- factor(df$year, levels = c("Before 2006", "After 2006"))

# Boxplot for visualizing the differences ######################################
ggplot(df, aes(x = year, y = efficiency)) +
  geom_boxplot(fill = c("#2166ac", "#b2182b"), alpha = 0.7, width = 0.5) +  
  stat_boxplot(geom = 'errorbar', size = 0.5, width = 0.2, color ='black') +  
  stat_summary(fun = mean, geom = 'point', pch = 4, color = 'black', size = 0.5) +  
  ylab("Cropland equipped \n for irrigation (%)") +  # Y-axis label
  theme_bw() +  # Use the white background theme
  theme(panel.background = element_rect(colour = "black", size = 0.5),  
        panel.grid = element_blank()) +  
  theme(axis.title = element_text(size = 9),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm")),  
        axis.ticks.length = unit(-0.1, "cm"),  
        axis.ticks = element_line(color = "black", size = 0.2),  
        axis.text = element_text(size = 9, color = "black"),  
        axis.text.x = element_text(margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm")),  
        axis.text.y = element_text(margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))) +  
  geom_signif(comparisons = list(c("Before 2006", "After 2006")),  
              map_signif_level = TRUE, test = "wilcox.test") + 
  scale_y_continuous(limits = c(54, 58.5), 
                     expand = c(0, 0))  

ggsave(filename="SignificantTesting.png",height=5, width=6, units="cm",dpi = 600)
