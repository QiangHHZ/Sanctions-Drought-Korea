setwd("..")
setwd("/Users/Qiang/Documents/R/Work/Drought")
getwd()

# set the packages
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

#
# land area equipped for irrigation ############################################
# import and inspect the data
data <- read_excel("NK_SK_Fig2_data.xlsx")
glimpse(data)

data_select <- dplyr::select(data,year,NK_LandAreaEquippedForIrrigation,SK_LandAreaEquippedForIrrigation)
data_select_slice <- data_select[-c(1:11),]
data_select_slice

before_2006 <- data_select_slice$NK_LandAreaEquippedForIrrigation[data_select_slice$year < 2006]
after_2006 <- data_select_slice$NK_LandAreaEquippedForIrrigation[data_select_slice$year >= 2006]

# Mann-Whitney U Test
wilcox_test_result <- wilcox.test(before_2006, after_2006)
print(wilcox_test_result)

# dataframe
df <- data.frame(
  year = rep(c("Before 2006", "After 2006"), c(length(before_2006), length(after_2006))),
  efficiency = c(before_2006, after_2006)
)
df$year <- factor(df$year, levels = c("Before 2006", "After 2006"))


# boxplot
ggplot(df, aes(x = year, y = efficiency)) +
  geom_boxplot(fill = c("#2166ac", "#b2182b"), alpha = 0.7, width = 0.5) +
  stat_boxplot(geom = 'errorbar', size = 0.5, width = 0.2, color ='black') +
  stat_summary(fun=mean, geom='point',pch=4,color='black', size=0.5) +
  # geom_jitter(aes(col = c("#2166ac", "#b2182b")), width = 0.3, shape = 16, size = 0.2, alpha= 0.1)+
  
  ylab("Cropland equipped \n for irrigation (%)") +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size = 0.5),
        panel.grid = element_blank() 
  )+   
  theme(axis.title = element_text(size = 9),   
        #axis.title.x = element_text(margin=unit(c(0.01,0.01,0.01,0.01), "cm")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=unit(c(0.01,0.01,0.01,0.01), "cm")),
        axis.ticks.length=unit(-0.1, "cm"),  
        axis.ticks = element_line(color = "black",size = 0.2),   
        axis.text = element_text(size = 9,color="black"),   
        axis.text.x = element_text(margin=unit(c(0.05,0.05,0.05,0.05), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.05,0.05,0.05,0.05), "cm")) 
  ) +   
  # geom_signif(comparisons = list(c("Before 2006", "After 2006")),
  #             map_signif_level = TRUE) +
  geom_signif(comparisons = list(c("Before 2006", "After 2006")),
              map_signif_level = TRUE, test = "wilcox.test")+
  scale_y_continuous(limits = c(54,58.5),
                     # breaks = c(),
                     # labels = c("0.4","","0.8","","1.2","","1.6"),
                     expand = c(0,0))

# ggsave(filename="Fig2_wue_line_SignificantTesting_equipped.png",height=5, width=6, units="cm",dpi = 600)
