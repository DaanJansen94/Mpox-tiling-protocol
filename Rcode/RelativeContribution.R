####################################################################################
# Relative_Contribution
####################################################################################
# Install and load necessary packages
# install.packages("ggplot2")
library(ggplot2)
library(esquisse)
library(reshape2)

# Set direcory
setwd("/Users/djansen/Library/CloudStorage/OneDrive-ITG/Desktop/Test_Combo_RC/Relative_contribution")
dir()

# Read the data
data1 <- read.table("bPM_output.txt", header = TRUE)
data1$start <- NULL
data1$end <- NULL
data1$depth <- NULL
data1$Bases_per_sample <- NULL

# Read the data
#esquisser(data1)

# Reorder the levels of Tile0 chronologically
data1$Tile0 <- factor(data1$Tile0, levels = rev(unique(data1$Tile0)))
data1$Names <- factor(data1$Names, levels = c("Welkers_T1", "Welkers_Pedro_T1", levels(data1$Names)))

# Figure 1
ggplot(data1) +
  aes(x = reorder(Tile0, -as.numeric(Tile0)), fill = Names, weight = bPM) +  # Reorder Tile0 chronologically
  geom_bar(position = "fill") +
  scale_fill_manual(values = c(Welkers_T1 = "#1A73A9", Welkers_Pedro_T1 = "#B1B108")) +
  labs(x = " ", y = "Relative Contribution (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format y-axis labels as percentages
  theme_bw() +theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, size = 7))  # Rotate x-axis labels by 45 degrees and adjust size

# Figure 2
ggplot(data1) +
  aes(x = reorder(Tile0, -as.numeric(Tile0)), fill = Names, weight = bPM) +  # Reorder Tile0 chronologically
  geom_bar(position = "fill", width = 0.9) +  # Adjust width of bars to fill space without gaps
  scale_fill_manual(values = c(Welkers_Pedro_T1 = "#249298", Welkers_T1 = "#F0F0F0")) +
  labs(x = " ", y = "Relative Contribution (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +  # Set the expansion limits of the y-axis
  geom_hline(yintercept = 0.5, linetype = 2, color = "black") +  # Add horizontal line at 50%
  geom_hline(yintercept = 0.9, linetype = 2, color = "black") +  # Add horizontal line at 50%
    theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_rect(colour = "black", fill = NA, size = 0.6),  # Set border around plot
        plot.background = element_rect(fill = "white"))  # Set background color of plot

# --> Cutoff from what point do we say this is usefull to add primer? 
# --> Gebruik log10 + 90% + remove 'tile'
####################################################################################