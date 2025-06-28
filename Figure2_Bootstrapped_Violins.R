library(ggplot2)
library(dplyr)
library(see)
library(ggridges)
library(dplyr)

set.seed(123)
setwd('Set working directory')
#read_file <- './data/Figure2_hallmark_ifna_toR.csv'
read_file <- './data/Figure2_hallmark_ifnb_toR.csv'

data <- read.delim(read_file, header=T, sep=",")

#### MAKE PLOT ####
hallmark <- head(data$hallmark, 1)

custom_colors <- c(
  'IFNb_Ctrl_2h' = '#1f77b4',
  'IFNb_ABR1000_2h' = '#ff7f0e',
  'IFNb_BAR1000_2h' = '#2ca02c',
  'IFNb_BMS1000_2h' = '#d62728',
  'IFNb_DEU1000_2h' = '#9467bd',
  'IFNb_MOM1000_2h' = '#8c564b',
  'IFNb_RUX1000_2h' = '#e377c2',
  'Unstim_Ctrl_2h' = 'gray'
)

sample_order = c('IFNb_Ctrl_2h','IFNb_ABR1000_2h', 'IFNb_BAR1000_2h', 'IFNb_BMS1000_2h', 'IFNb_DEU1000_2h', 'IFNb_MOM1000_2h', 'IFNb_RUX1000_2h', 'Unstim_Ctrl_2h')
data$treatment <- factor(data$treatment, levels = (sample_order))
# Ensure the 'treatment' factor includes all treatments for proper dodging
data$treatment <- factor(data$treatment, levels = sample_order)

# Group data by CellType and treatment to get the maximum mean_value for each group
max_values <- data %>%
  group_by(CellType, treatment) %>%
  summarize(max_mean_value = max(mean_value))

# Merge max_values back into the original data
data <- merge(data, max_values, by = c("CellType", "treatment"))

#Set some margin
max_y_value <- max(data$mean_value) +1

dodge_width <- 1

p <- ggplot(data, aes(x = CellType, y = mean_value, fill = treatment)) +
  geom_violin(alpha = 0.5, position = position_dodge(width = dodge_width), scale = "width", width = 0.8, color = NA) +  # Adjusted width
  geom_jitter(aes(color = CellType), 
              position = position_jitterdodge(jitter.width = 0.05, dodge.width = dodge_width),  # Reduced jitter width
              alpha = 0.4, shape = 21, stroke = 0.01, size = 0.2, color = "black") +  # Reduced size
  scale_y_continuous(name = "", limits = c(0, max_y_value)) + 
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  geom_vline(xintercept = seq(1.5, length(unique(data$CellType)) - 0.5, by = 1), 
             color = "gray", linetype = "dashed", alpha = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), #size = 30
    axis.text.y = element_text(size = 10), #size = 30
    legend.position = "None", #right
    legend.text = element_text(size = 25), 
    legend.title = element_text(size = 20), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),  # Make the panel background transparent
    plot.background = element_rect(fill = "transparent", color = NA),   # Make the plot background transparent
  ) 


p





