setwd("..")
setwd("/Users/Qiang/Documents/R/Work/Drought/Visualization")
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

library(tidyr)
library(dplyr)

# rice yield #####################################################################
#*******************************************************************************
# import and inspect the data
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)

data <- data %>% 
  select(year, NK_RiceYield, SK_RiceYield) 
glimpse(data)

data <- data[-c(1:15),]
glimpse(data)

# 计算标准差
nk_sd <- sd(data$NK_RiceYield)
nk_sd
sk_sd <- sd(data$SK_RiceYield)
sk_sd

# 计算均值
nk_mean <- mean(data$NK_RiceYield)
nk_mean
sk_mean <- mean(data$SK_RiceYield)
sk_mean

# 计算变异系数
nk_cv <- nk_sd / nk_mean
nk_cv
sk_cv <- sk_sd / sk_mean
sk_cv

#
data_long <- pivot_longer(data, cols = 2:3, names_to = "rice", values_to = "value")
head(data_long)
data_long$rice <- factor(data_long$rice,
                         levels = c("NK_RiceYield","SK_RiceYield"),
                         labels = c("NK", "SK"))
glimpse(data_long)

# 创建一个新的数据框，添加插值后的数据点，以便平滑连接
df_smoothed <- data_long %>%
  group_by(rice) %>%
  do(data.frame(year = seq(min(.$year), max(.$year), length.out = 10000),
                smooth_value = approx(.$year, .$value, xout = seq(min(.$year), max(.$year), length.out = 10000))$y))
glimpse(df_smoothed)
# data visualization
#series
p_rice <- ggplot(data_long, aes(year, value))+ 
  annotate("text", size = 2, x=2011, y=2.8, label="CV = 0.20", color="#DE2D26")+
  annotate("text", size = 2, x=2011, y=7.8, label="CV = 0.05", color="#174994")+
  
  geom_line(aes(color=rice), size = 0.6, alpha=0.9)+
  scale_color_manual(breaks = c("NK","SK"),
                     labels = c("DPRK","ROK"), 
                     values = c("#DE2D26","#174994")) +
  
  theme_bw() +
  theme(#panel.border = element_blank(),
    # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
    panel.background = element_blank(),
    panel.grid = element_blank() #去掉背景网格
  )+   
  theme(#legend.position = c(0.2, 0.22),    #图例位置
    legend.position = c(0, 1),legend.justification = c(0,1),
    
    legend.background = element_blank(),
    legend.key = element_blank(),
    #legend.key.size = unit(15,"pt"),   #图例大小
    legend.key.height = unit(2,"pt"),   #图例大小
    legend.key.width = unit(10,"pt"),   #图例大小
    legend.title = element_blank(),   #去掉图例的题目
    legend.text = element_text(size = 7) #图例字体大小
  ) + 
  
  theme(axis.title = element_text(size = 7),    #坐标轴标题
        #axis.title.x = element_text(margin=unit(c(0.01,0.01,0.01,0.01), "cm")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=unit(c(0.01,0.01,0.01,0.01), "cm")),
        
        # axis.line = element_line(color = "black"),
        
        axis.ticks.length=unit(0.1, "cm"),   #刻度长度
        axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
        axis.text = element_text(size = 7,color="black"),    #坐标轴标识
        axis.text.x = element_text(margin=unit(c(0.05,0.05,0.05,0.05), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.05,0.05,0.05,0.05), "cm")) #坐标轴刻度向内，调整刻度和标签的距离
  ) +   
  
  xlab("Year") + 
  # ylab("Rice yield \n (t/ha)")+
  ylab(expression(atop("Rice yield", "(t/ha)"))) + 
  # xlab("Year") + ylab("Rice yield ("~10^6~"t)")+
  
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980, 1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(1,9),breaks = c(2,4,6,8),
                     labels = c("2","4","6","8"),expand = c(0,0))

p_rice

# land area equipped for irrigation #####################################################################
#*******************************************************************************
library(gtable)
library(grid)

#
# p_equipped--------------------------------------------------------------------
# import and inspect the data
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)
data_select <- dplyr::select(data,year, 
                      NK_LandAreaEquippedForIrrigation, SK_LandAreaEquippedForIrrigation)
data_select
#
data_long_equipped <- pivot_longer(data_select, cols = 2:3, names_to = "equipped", values_to = "value_equipped")
head(data_long_equipped)
#
p_equipped <- ggplot(data_long_equipped) + 
  geom_line(aes(x = year, y = value_equipped, color = equipped), size = 0.6, alpha=0.9)+
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("NK_LandAreaEquippedForIrrigation","SK_LandAreaEquippedForIrrigation"),
                     labels = c("DPRK","ROK"), 
                     values = c("#DE2D26","#174994")
                     )+
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(38,60),breaks = c(40,45,50,55),
                     labels = c("40","45","50","55"),expand = c(0,0))+
  # ylab("Cropland equipped \n for irrigation (%)")+
  ylab(expression(atop("Cropland equipped", "for irrigation (%)"))) + 
  
  theme_bw()+
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
        )+
  theme(legend.position = "none",    #图例位置
        # legend.position = c(0, 0),legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.box.background = element_blank(),  # 移除图例的黑色边框
        legend.key = element_blank(),
        #legend.key.size = unit(15,"pt"),   #图例大小
        legend.key.height = unit(2,"pt"),   #图例大小
        legend.key.width = unit(12,"pt"),   #图例大小
        legend.title = element_blank(),   #去掉图例的题目
        legend.text = element_text(size = 5) #图例字体大小
        ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black"),
        axis.ticks.length=unit(0.1, "cm"),   #刻度长度
        axis.ticks = element_line(color = "black",size = 0.2)   #刻度粗细
        )

p_equipped

# p_dam--------------------------------------------------------------------
# import and inspect the data
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)
data_select <- dplyr::select(data,year, 
                      NK_TotalDamCapacity, SK_TotalDamCapacity)
data_select
#
data_long_dam <- pivot_longer(data_select, cols = 2:3, names_to = "dam", values_to = "value_dam")
head(data_long_dam)
#
p_dam <- ggplot(data_long_dam) + 
  geom_line(aes(x = year, y = value_dam, color = dam), size = 0.6, alpha=0.9)+
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("NK_TotalDamCapacity","SK_TotalDamCapacity"),
                     labels = c("DPRK","ROK"), 
                     values = c("#DE2D26","#174994")
  )+
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(7,22),breaks = c(12,16,20),
                     labels = c("12","16","20"),expand = c(0,0))+
  # ylab(expression("Total dam capacity ("~km^3~")")) +
  ylab(expression(atop("Total dam capacity", "(km"^3*")"))) + 
  
  theme_bw()+
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
  )+
  # theme(strip.background = element_blank(), 
  #       plot.background = element_blank(),
  #       panel.background = element_blank())+
  theme(legend.position = "none",
        #legend.position = c(0.2, 0.22),    #图例位置
    # legend.position = c(0, 0),legend.justification = c(0,0),
    legend.background = element_blank(),
    legend.box.background = element_blank(),  # 移除图例的黑色边框
    legend.key = element_blank(),
    #legend.key.size = unit(15,"pt"),   #图例大小
    legend.key.height = unit(2,"pt"),   #图例大小
    legend.key.width = unit(12,"pt"),   #图例大小
    legend.title = element_blank(),   #去掉图例的题目
    legend.text = element_text(size = 5) #图例字体大小
  ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length=unit(0.1, "cm"),   #刻度长度
        axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_dam
#

# p_WUE--------------------------------------------------------------------
# import and inspect the data
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)
data_select <- dplyr::select(data,year, 
                      NK_IrrigatedAgricultureWaterUseEfficiency,SK_IrrigatedAgricultureWaterUseEfficiency)
data_select
#
data_long_wue <- pivot_longer(data_select, cols = 2:3, names_to = "wue", values_to = "value_wue")
head(data_long_wue)
#
p_wue <- ggplot(data_long_wue) + 
  geom_line(aes(x = year, y = value_wue, color = wue), size = 0.6, alpha=0.9)+
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("NK_IrrigatedAgricultureWaterUseEfficiency","SK_IrrigatedAgricultureWaterUseEfficiency"),
                     labels = c("DPRK","ROK"), 
                     values = c("#DE2D26","#174994")
  )+
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(0.3,1.7),breaks = c(0.6,1.0,1.4),
                     labels = c("0.6","1.0","1.4"),expand = c(0,0))+
  # ylab(expression("WUE (dollars /"~m^3~")")) +
  ylab(expression(atop("Irrigation WUE", "(dollars/m"^3*")"))) + 
  
  theme_bw()+
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
  )+
  # theme(strip.background = element_blank(), 
  #       plot.background = element_blank(),
  #       panel.background = element_blank())+
  theme(legend.position = "none",
        #legend.position = c(0.2, 0.22),    #图例位置
    # legend.position = c(0, 0),legend.justification = c(0,0),
    legend.background = element_blank(),
    legend.box.background = element_blank(),  # 移除图例的黑色边框
    legend.key = element_blank(),
    #legend.key.size = unit(15,"pt"),   #图例大小
    legend.key.height = unit(2,"pt"),   #图例大小
    legend.key.width = unit(12,"pt"),   #图例大小
    legend.title = element_blank(),   #去掉图例的题目
    legend.text = element_text(size = 5) #图例字体大小
  ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length=unit(0.1, "cm"),   #刻度长度
        axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )

p_wue
#

# p_WCC--------------------------------------------------------------------
# import and inspect the data
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)
data_select <- dplyr::select(data,year, 
                      NK_WaterConsumptionCoefficient, SK_WaterConsumptionCoefficient)
data_select
#
data_long_wcc <- pivot_longer(data_select, cols = 2:3, names_to = "wcc", values_to = "value_wcc")
head(data_long_wcc)
#
p_wcc <- ggplot(data_long_wcc) + 
  geom_line(aes(x = year, y = value_wcc, color = wcc), size = 0.6, alpha=0.9)+
  
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  scale_color_manual(breaks = c("NK_WaterConsumptionCoefficient","SK_WaterConsumptionCoefficient"),
                     labels = c("DPRK","ROK"), 
                     values = c("#DE2D26","#174994")
  )+
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(0.2,1.65),breaks = c(0.6,1.0,1.4),
                     labels = c("0.6","1.0","1.4"),expand = c(0,0))+
  # ylab("Water consumption\ncoefficient") +
  ylab(expression(atop("Water consumption", "coefficient"))) + 
  
  theme_bw()+
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
  )+
  theme(legend.position = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank(),  # 移除图例的黑色边框
    legend.key = element_blank(),
    #legend.key.size = unit(15,"pt"),   #图例大小
    legend.key.height = unit(2,"pt"),   #图例大小
    legend.key.width = unit(12,"pt"),   #图例大小
    legend.title = element_blank(),   #去掉图例的题目
    legend.text = element_text(size = 5) #图例字体大小
  ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = "black"),
        axis.ticks.length=unit(0.1, "cm"),   #刻度长度
        axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")
  )
  

p_wcc
#

# electricity generation #####################################################################
#*******************************************************************************
# import and inspect the data
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)

data_select <- dplyr::select(data,year,NK_ElectricityGeneration, SK_ElectricityGeneration)
data_select

data_slice <- data_select
data_slice

data_long <- pivot_longer(data_select, cols = 2:3, names_to = "elec", values_to = "value")
head(data_long)


# data visualization 朝鲜+韩国
#series
p_electricity <- ggplot(data_long)+ 
  
  #geom_rect(aes(xmin=6, xmax=9, ymin=-Inf, ymax=Inf),fill='#fee8c8',alpha = 0.3)+
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  #geom_smooth(se = T, method = 'loess') + #绘制拟合曲线
  #geom_point() + 
  geom_line(aes(x = year, y = value, color= elec), size = 0.6, alpha=0.9)+
  
  ylab(expression(atop("Electricity generation", "(10"^11*"kWh)"))) + 
  
  # scale_x_continuous(name = "Year")+
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(0,6),breaks = c(1,3,5),
                     labels = c("1","3","5"),expand = c(0,0))+
  # geom_point(aes(), color= "#993404", size = 0.5, alpha=0.8,shape=16)+
  scale_color_manual(breaks = c("NK_ElectricityGeneration","SK_ElectricityGeneration"),
                     labels = c("DPRK","ROK"),
                     values = c("#DE2D26","#174994")) +
  # 
  theme_bw() +
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
  ) +
  theme(legend.position = "none"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7, color = "black"),
    axis.ticks.length=unit(0.1, "cm"),   #刻度长度
    axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black")
  )

p_electricity



# oil, coal ##############################################################################
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)
data_select <- dplyr::select(data,year, 
                      NK_CrudeOilImport, SK_CrudeOilImport)
data_select

data_slice <- data_select
data_slice

data_long <- pivot_longer(data_select, cols = 2:3, names_to = "oil", values_to = "value")
head(data_long)
# data visualization 朝鲜+韩国
#series
p_oil <- ggplot(data_long)+ 
  
  #geom_rect(aes(xmin=6, xmax=9, ymin=-Inf, ymax=Inf),fill='#fee8c8',alpha = 0.3)+
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  #geom_smooth(se = T, method = 'loess') + #绘制拟合曲线
  #geom_point() + 
  geom_line(aes(x = year, y = value, , color = oil), size = 0.6, alpha=0.9)+
  # geom_line(aes(y = value_coal/0.55, color = coal), size = 0.6, alpha=0.7)+
  
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(-0.1,3.1),breaks = c(1,2,3),
                     labels = c("1","2","3"),expand = c(0,0))+
  
  # ylab("Crude oil import\n(Mb/d)") + 
  ylab(expression(atop("Crude oil import", "(10"^3*"Mb/d)"))) + 
  
  scale_color_manual(breaks = c("NK_CrudeOilImport","SK_CrudeOilImport"),
                     labels = c("DPRK","ROK"),
                     values = c("#DE2D26","#174994")) +
  
  theme_bw() +
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
  )+
  
  theme(legend.position = "none",    #图例位置
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7, color = "black"),
    axis.ticks.length=unit(0.1, "cm"),   #刻度长度
    axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black")
    # axis.line = element_line(color = "black")
  )

p_oil


data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)
data_select <- dplyr::select(data,year,
                      NK_CoalImport,SK_CoalImport)
data_select

data_slice <- data_select
data_slice

data_long <- pivot_longer(data_select, cols = 2:3, names_to = "coal", values_to = "value")
head(data_long)

# 朝鲜+韩国
p_coal <- ggplot(data_long)+ 
  
  #geom_rect(aes(xmin=6, xmax=9, ymin=-Inf, ymax=Inf),fill='#fee8c8',alpha = 0.3)+
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  #geom_smooth(se = T, method = 'loess') + #绘制拟合曲线
  #geom_point() + 
  geom_line(aes(x = year, y = value, color = coal), size = 0.6, alpha=0.9)+
  # geom_line(aes(y = value_coal/0.55, color = coal), size = 0.6, alpha=0.7)+
  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) +
  scale_y_continuous(limits = c(-0.1,1.7),breaks = c(0.5,1.0,1.5),
                     labels = c("0.5","1.0","1.5"),expand = c(0,0))+
  
  # ylab("Coal import ("~10^3~"Mst)") +
  ylab(expression(atop("Coal import", "(10"^5*"Mst)"))) + 
  
  scale_color_manual(breaks = c("NK_CoalImport","SK_CoalImport"),
                     labels = c("DPRK","ROK"),
                     values = c("#DE2D26","#174994")) +
  
  theme_bw() +
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
  )+
  
  theme(legend.position = "none",    #图例位置
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7, color = "black"),
    axis.ticks.length=unit(0.1, "cm"),   #刻度长度
    axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black")
    # axis.line = element_line(color = "black")
  )

p_coal


# coal production #####################################################################
#*******************************************************************************
# import and inspect the data
data <- read_excel("Data_Statistics_R4.xlsx")
glimpse(data)

data_select <- data %>% 
  dplyr::select(year, NK_CoalProduction, SK_CoalProduction)
data_select

data_slice <- data_select
data_slice

data_long <- pivot_longer(data_slice, cols = 2:3, names_to = "coalprod", values_to = "value")
head(data_long)
data_long$coalprod <- factor(data_long$coalprod,
                        levels = c("NK_CoalProduction","SK_CoalProduction"))


# data visualization 朝鲜+韩国
#series
p_coalprod <- ggplot(data_long)+ 
  
  #geom_rect(aes(xmin=6, xmax=9, ymin=-Inf, ymax=Inf),fill='#fee8c8',alpha = 0.3)+
  geom_vline(xintercept = 1991, linetype = "dashed", size = 0.5, color = "#969696") +
  geom_vline(xintercept = 2006, linetype = "dashed", size = 0.5, color = "#969696") +
  
  #geom_smooth(se = T, method = 'loess') + #绘制拟合曲线
  #geom_point() + 
  geom_line(aes(x = year, y = value, color=coalprod), size = 0.6, alpha=0.9)+
  # geom_point(aes(color=crop), size = 0.5, alpha=0.8,shape=16)+
  scale_color_manual(breaks = c("NK_CoalProduction","SK_CoalProduction"),
                     labels = c("DPRK","ROK"),
                     values = c("#DE2D26","#174994")) +
  
  theme_bw() +
  theme(#panel.border = element_blank(),
        # panel.background = element_rect(colour = "black", size = 0.5), #图框颜色、粗细
        panel.grid = element_blank() #去掉背景网格
  )+   
  theme(legend.position = "none",    #图例位置
  ) + 
  
  theme(axis.title = element_text(size = 7),    #坐标轴标题
        # axis.title.x = element_text(margin=unit(c(0.01,0.01,0.01,0.01), "cm")),
        axis.title.x = element_blank(),
        # axis.title.y = element_text(margin=unit(c(0.01,0.01,0.01,0.01), "cm")),
        # axis.ticks.length=unit(-0.1, "cm"),   #刻度长度
        # axis.line = element_line(color = "black"),
        axis.ticks.length=unit(0.1, "cm"),   #刻度长度
        axis.ticks = element_line(color = "black",size = 0.2),   #刻度粗细
        # axis.ticks = element_line(color = "black",size = 0.5),   #刻度粗细
        axis.text = element_text(size = 7,color="black")    #坐标轴标识
  ) +   
  
  xlab("Year") + 
  # ylab("Coal production\n(10^4Mst)")+
  ylab(expression(atop("Coal production", "(10"^4*"Mst)"))) + 

  scale_x_continuous(limits = c(1980,2020),breaks = c(1980,1990,2000,2010,2020),
                     labels = c("1980","1990","2000","2010","2020")) 

p_coalprod



# 组合--------------------------------------------------------------------------
library("Rmisc")
library("ggpubr")
library("patchwork")

plot_grid(p_rice, p_equipped, p_dam, p_wue, p_wcc, p_electricity, p_coalprod, p_oil, p_coal,  # p_rice are in Fig.1_sdg_Revision.R
          ncol = 3, 
          align = "v")
#
ggsave(filename="Fig.2_line_combine_Revision4_R4.png",height=9, width=15, units="cm",dpi = 600)
