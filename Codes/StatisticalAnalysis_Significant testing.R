# Working space
setwd("your path")

# Packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(patchwork)

# Load data
raw_data <- read_excel("./Data/Data_Statistics.xlsx")

data_irrigation <- raw_data %>% 
  select(year, DPRK_LandAreaEquippedForIrrigation, DPRK_IrrigatedAgricultureWaterUseEfficiency) %>% 
  filter(year >= 1995)

# Function for Wilcoxon test and boxplot
create_boxplot <- function(df, var, y_label, y_limits, filename) {
  df_plot <- df %>% 
    mutate(period = if_else(year < 2006, "Before 2006", "After 2006")) %>% 
    select(period, value = {{ var }}) %>% 
    drop_na()
  
  # Perform Wilcoxon test
  test_result <- wilcox.test(value ~ period, data = df_plot)
  print(paste("Wilcoxon test p-value for", deparse(substitute(var)), ":", test_result$p.value))
  
  # Create boxplot
  p <- ggplot(df_plot, aes(x = factor(period, levels = c("Before 2006", "After 2006")), y = value)) +
    geom_boxplot(fill = c("#2166ac", "#b2182b"), alpha = 0.7, width = 0.5) +
    stat_boxplot(geom = 'errorbar', width = 0.2, size = 0.5, color = 'black') +
    stat_summary(fun = mean, geom = 'point', shape = 4, color = 'black', size = 0.5) +
    geom_signif(comparisons = list(c("Before 2006", "After 2006")), test = "wilcox.test", map_signif_level = TRUE) +
    ylab(y_label) +
    theme_bw() +
    theme(
      panel.background = element_rect(colour = "black", size = 0.5),
      panel.grid = element_blank(),
      axis.title = element_text(size = 9),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(1, 1, 1, 1, "mm")),
      axis.ticks.length = unit(-0.1, "cm"),
      axis.ticks = element_line(color = "black", size = 0.2),
      axis.text = element_text(size = 9, color = "black"),
      axis.text.x = element_text(margin = margin(0.5, 0.5, 0.5, 0.5, "mm")),
      axis.text.y = element_text(margin = margin(0.5, 0.5, 0.5, 0.5, "mm"))
    ) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0))
  
  return(p)
}

# Plot: Cropland equipped for irrigation
p1 <- create_boxplot(
  data_irrigation,
  DPRK_LandAreaEquippedForIrrigation,
  "Cropland equipped\nfor irrigation (%)",
  c(54, 58.5),
  "Fig2_irrigation.png"
)

# Plot: Irrigated agriculture water use efficiency
p2 <- create_boxplot(
  data_irrigation,
  DPRK_IrrigatedAgricultureWaterUseEfficiency,
  expression("WUE (dollars /"~m^3~")"),
  c(0.32, 0.48),
  "Fig2_wue.png"
)

# Combine and export
combined_plot <- ggarrange(p1, p2, ncol = 2, labels = "auto", align = "v",
                           font.label = list(size = 12, color = "black", face = "plain"))
combined_plot

ggsave("Fig. S19_SignificantTesting.png", plot = combined_plot, height = 5, width = 12, units = "cm", dpi = 600)
