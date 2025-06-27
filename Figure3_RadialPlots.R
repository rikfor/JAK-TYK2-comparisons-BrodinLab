library(fmsb)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ragg)
library(tidyverse)

treatment_list <- c("ABR1000", "BAR1000", "BMS1000", "DEU1000", "MOM1000", "RUX1000")
cell_types_list <- c("B", "CD4T", "CD8T", 
                     "Neutrophil", "Monocyte", "NK")


# Define treatment colors
colors <- c(
  'ABR1000' = '#F37F20',
  'BAR1000' = '#2F9F48',
  'BMS1000' = '#D52B29',
  'DEU1000' = '#9269AB',
  'MOM1000' = '#8C584D',
  'RUX1000' = '#D77BB0'
)


df_res <- read.csv('./data/Figure3_radialPlots_toR.csv')

unique(df_res$hallmark)

# Rename hallmark labels for final plot
rename_hallmarks <- function(label) {
  replacements <- c(
    "hallmark_ifna" = "IFNa",
    "hallmark_ifnb" = "IFNb",
    "hallmark_ifng" = "IFNg",
    "hallmark_tnf" = "TNFa",
    'hallmark_nfkb' = 'NFkB',
    "HLAs-Type I" = "HLAs\nType I",
    "IL8_CXCR1/2" = "IL8",
    "IL1" = "IL1",
    "IL4" = "IL4",
    "IL12" = "IL12",
    "IL17" = "IL17",
    "IL18" = "IL18",
    "IL23" = "IL23"
  )
  return(replacements[label])
}
df_res$hallmark <- sapply(df_res$hallmark, rename_hallmarks)


#Set order of the cytokines in the plot
df_res$hallmark <- factor(df_res$hallmark, levels = c('IFNa', 'IFNb', 'IFNg', 'TNFa', 'NFkB', 
                                                      'IL1', 'IL18', 'IL4', 'IL8', 'IL12', 'IL17', 'IL23', 'HLAs\nType I'))
cyto_order <- levels(df_res$hallmark)

# Function to create a single radar plot with dynamic axis
create_radar_plot <- function(data_subset, title = "", min_value, max_value, axis_labels, color) {
  
  # Reshape data for fmsb format
  plot_data <- data_subset %>%
    pivot_wider(names_from = hallmark, values_from = log2FC) %>%
    select(-treatment)
  
  # Add max and min rows required by fmsb to define the scale
  plot_data <- rbind(rep(max_value, ncol(plot_data)),  # Set max scale
                     rep(min_value, ncol(plot_data)),  # Set min scale
                     plot_data)
  
  radarchart(plot_data,
             axistype = 1,
             pcol = color,
             pfcol = adjustcolor(color, alpha.f = 0.5), 
             plty = 1, 
             cglcol = "grey",
             cglty = 1,
             axislabcol = "transparent", #black
             caxislabels = axis_labels, #axis_labels
             cglwd = 0.8, #line width for radar grids
             vlcex = 1.2, #value labels on the axes (numbers) 
             palcex=1.1,
             main.cex = 1.4, #Title size
             title = title,
             axislabels = FALSE,  #FALSE # We'll add our own labels
             seg = length(axis_labels) - 1,
             r_nudge_caxislabels = 1.15,
             r_nudge_vlabels = 1.15,
             family = "Arial"
  )
  
  # # Get unique column names (excluding the first two rows which are min/max)
  unique_labels <- unique(colnames(plot_data))
  n_vars <- length(unique_labels)
  
  # Calculate angles for labels
  angles <- seq(90, 450, length.out = n_vars + 1)[1:n_vars]  # Start from top (90 degrees)
  angles_rad <- angles * pi / 180  # Convert to radians
  
  axis_positions <- seq(0, 0.9, length.out = length(axis_labels))
  #for(i in 1:length(axis_labels)) {
    for (i in seq(2, length(axis_labels), by = 2)) {  # Only plot every other label
      text(0, axis_positions[i]+0.12, labels = axis_labels[i],
         col = "black", cex = 1.2, pos = 2, font = 2)#, family = 'Arial')
  }
  
}

# Create layout matrix for 6x6 grid
layout(matrix(1:36, 6, 6, byrow = TRUE))
tick_steps <- list(
  "B" = seq(-1.25, 0.75, by = 0.5),
  "CD4T" = seq(-1.25, 0.75, by = 0.25),
  "CD8T" = seq(-1.25, 0.75, by = 0.25),
  "Neutrophil" = seq(-1.25, 0.75, by = 0.25),
  "Monocyte" = seq(-1.25, 0.75, by = 0.25),
  "NK" = seq(-1.25, 0.75, by = 0.25)
)

par(xpd = TRUE, mfrow = c(6, 6), mar = c(1, 1, 1, 1), mai = c(0.1, 0.05, 0.1, 0.05))
#Create plot
for(cell in cell_types_list) {
  for(cond in treatment_list) {
    plot_data <- df_res[df_res$CellType == cell, ] %>% select(-X, -mean_value, -CellType, -pvalue, -perc_found, -pvalue_adj)
    
    plot_data <- plot_data[plot_data$treatment == cond, ]
    plot_data <- arrange(plot_data, hallmark)
    
    axis_labels <- seq(-1.25, 0.75, by = 0.25)
    
    global_min <- min(axis_labels)
    global_max <- max(axis_labels)
    color <- colors[cond]
    
    create_radar_plot(plot_data, title = paste(cell, "-", cond), min_value = global_min, max_value = global_max, axis_labels = axis_labels, color = color)
  }
}

