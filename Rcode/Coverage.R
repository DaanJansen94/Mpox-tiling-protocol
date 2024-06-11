####################################################################################
# First obtain depth from bam file
####################################################################################
# samtools depth -a Combo1_T1.NC_003310_1.sorted.bam > Combo1.coverage.txt 
# Use this Combo1.coverage.txt in Rstudio
####################################################################################
# 50x
####################################################################################
# Install and load necessary packages
# install.packages("ggplot2")
library(ggplot2)
library(esquisse)

# Set direcory
setwd("/Users/djansen/Library/CloudStorage/OneDrive-ITG/Desktop/Test_Combo_RC/HC")
dir()

# Read the data
data1 <- read.table("Welkers.NC_003310_1.txt", header = FALSE)
data2 <- read.table("Welkers_Pedro_T1.NC_003310_1.txt", header = FALSE)

# Assign column names
colnames(data1) <- c("reference", "position", "depth")
colnames(data2) <- c("reference", "position", "depth")

# Change depth to numericals
data1$depth <- as.numeric(data1$depth)
data2$depth <- as.numeric(data2$depth)

# Take logarithm of depth values, replacing -Inf with zero
data1$log_depth <- log10(data1$depth)
data1$log_depth[is.infinite(data1$log_depth)] <- 0

data2$log_depth <- log10(data2$depth)
data2$log_depth[is.infinite(data2$log_depth)] <- 0

# Colors
data1$color <- ifelse(data1$depth > 50, "Above 50", "Below 50")
data2$color <- ifelse(data2$depth > 50, "Above 50", "Below 50")

# Add a new column with the same value 'Combo1'
data1$name <- "Welkers"
data2$name <- "Welkers_Pedro"

# Combine both datasets vertically
combined_data <- rbind(data1, data2)

####################################################################################
# Make less computationally heave an bin
####################################################################################
# Bin the data per 10 positions and calculate the mean depth within each bin
library(dplyr)
library(ggplot2)
library(stringr)

combined_data_binned <- combined_data %>%
  mutate(bin = cut(position, breaks = seq(0, max(position) + 10, by = 10), include.lowest = FALSE)) %>%
  group_by(name, bin) %>%
  summarise(avg_depth = mean(log_depth))

combined_data_binned$bin_start <- combined_data_binned$bin
combined_data_binned$bin_start <- gsub("[\\[\\(\\)]", "", combined_data_binned$bin_start)
combined_data_binned$bin_start <- gsub("\\[|\\]", "", combined_data_binned$bin_start)
combined_data_binned$bin_start <- sub(",.*", "", combined_data_binned$bin_start)
combined_data_binned <- as.data.frame(combined_data_binned)
combined_data_binned$bin_start <- as.numeric(combined_data_binned$bin_start)

ggplot(combined_data_binned) +
  aes(x = bin_start, y = avg_depth) +
  geom_line(color = "#057215") +
  geom_segment(data = subset(combined_data_binned, avg_depth < 1.69), 
               aes(x = bin_start, xend = bin_start, y = 0, yend = -0.3), 
               color = "darkred", 
               alpha = 0.2, 
               linewidth = 1) +
  geom_hline(yintercept = 1.69, linetype = "dashed", color = "black") +
  theme_bw() +
  facet_wrap(vars(name)) +
  labs(title = "Coverage Distribution",
       x = "Position",
       y = "Log10(Depth)")


# Bin the data per 50 positions and calculate the mean depth within each bin
library(dplyr)
library(ggplot2)
library(stringr)

combined_data_binned <- combined_data %>%
  mutate(bin = cut(position, breaks = seq(0, max(position) + 50, by = 50), include.lowest = FALSE)) %>%
  group_by(name, bin) %>%
  summarise(avg_depth = mean(log_depth))

combined_data_binned$bin_start <- combined_data_binned$bin
combined_data_binned$bin_start <- gsub("[\\[\\(\\)]", "", combined_data_binned$bin_start)
combined_data_binned$bin_start <- gsub("\\[|\\]", "", combined_data_binned$bin_start)
combined_data_binned$bin_start <- sub(",.*", "", combined_data_binned$bin_start)
combined_data_binned <- as.data.frame(combined_data_binned)
combined_data_binned$bin_start <- as.numeric(combined_data_binned$bin_start)

ggplot(combined_data_binned) +
  aes(x = bin_start, y = avg_depth) +
  geom_line(color = "#057215") +
  geom_segment(data = subset(combined_data_binned, avg_depth < 1.69), 
               aes(x = bin_start, xend = bin_start, y = 0, yend = -0.3), 
               color = "darkred", 
               alpha = 0.2, 
               size = 1) +
  geom_hline(yintercept = 1.69, linetype = "dashed", color = "black") +
  theme_bw() +
  facet_wrap(vars(name)) +
  labs(title = "Coverage Distribution",
       x = "Position",
       y = "Log10(Depth)")

# --> correct for bases
####################################################################################