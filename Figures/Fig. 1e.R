# working space
setwd("..")
setwd("your path")
getwd()

# packages
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

# ------------------ SPI ------------------ #
spi_data <- read_excel("./Data/Data_MonthSeries_SPI.xlsx")
glimpse(spi_data)

spi_data <- spi_data %>% select(month, DPRK_SPI_2015, ROK_SPI_2015)

spi_long <- pivot_longer(spi_data, cols = 2:3, names_to = "spi", values_to = "spi_value")
glimpse(spi_long)

spi_long$spi <- factor(spi_long$spi,
                       levels = c("DPRK_SPI_2015", "ROK_SPI_2015"),
                       labels = c("DPRK_SPI", "ROK_SPI"))
glimpse(spi_long)


# ------------------ CSIF ------------------ #
csif_data <- read_excel("./Data/Data_MonthSeries_CSIF.xlsx")
glimpse(csif_data)

csif_data <- csif_data %>% select(month, DPRK_relativechange, ROK_relativechange)

csif_long <- pivot_longer(csif_data, cols = 2:3, names_to = "csif", values_to = "csif_value")

csif_long$csif <- factor(csif_long$csif,
                         levels = c('DPRK_relativechange', 'ROK_relativechange'),
                         labels = c("DPRK_CSIF", "ROK_CSIF"))

glimpse(csif_long)

# ------------------ Data merging ------------------ #
merged_data <- merge(spi_long, csif_long, by = "month", all = TRUE)
glimpse(merged_data)

# ------------------ Visualization ------------------ #
p_spi_csif <- ggplot(merged_data, aes(month)) +
  geom_rect(aes(xmin = 6, xmax = 9, ymin = -Inf, ymax = Inf), fill = '#fee8c8', alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.1, color = "red") +
  
  geom_line(aes(y = spi_value, color = spi), size = 0.5, alpha = 0.9) +
  geom_point(aes(y = spi_value, color = spi), size = 1, alpha = 0.9) +
  geom_line(aes(y = csif_value / 11, color = csif), size = 0.5, alpha = 0.9) +
  geom_point(aes(y = csif_value / 11, color = csif), size = 1, alpha = 0.9) +
  
  scale_y_continuous(
    name = bquote(SPI[TerraClimate]),
    sec.axis = sec_axis(~ . * 11, name = expression("CSIF relative change (%)"))
  ) +
  
  scale_x_continuous(limits = c(1, 12), breaks = 1:12) +
  xlab("Month") +
  theme_bw() +
  
  scale_color_manual(breaks = c("DPRK_SPI", "ROK_SPI", "DPRK_CSIF", "ROK_CSIF"),
                     labels = c("DPRK", "ROK", "DPRK", "ROK"),
                     values = c("#b2182b", "#fc9277", "#2166ac", "#9ecae1")) +
  
  theme(
    panel.grid = element_blank(),
    legend.position = c(0, 0),
    legend.justification = c(0, 0),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    axis.title.x = element_text(size = 9, color = "black"),
    axis.title.y.left = element_text(size = 9, color = "#b2182b"),
    axis.title.y.right = element_text(size = 9, color = "#2166ac"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y.left = element_text(size = 9, color = "#b2182b"),
    axis.text.y.right = element_text(size = 9, color = "#2166ac"),
    axis.ticks.y.left = element_line(color = "#b2182b"),
    axis.ticks.y.right = element_line(color = "#2166ac"),
    axis.line.y.left = element_line(color = "#b2182b"),
    axis.line.y.right = element_line(color = "#2166ac")
  )

p_spi_csif

# Save 
ggsave(filename = "Fig. 1e_SPI_CSIF_lineplot.png", height = 5, width = 8.5, units = "cm", dpi = 600)
