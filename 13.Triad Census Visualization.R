# Load necessary libraries
library(igraph)
library(ggplot2)
library(dplyr)
library(scales)

# Define a function to calculate and visualize triad census statistics for a single network
plot_triad_census <- function(net, net_name) {
  # Calculate triad census statistics
  triad_census_result <- triad.census(net)
  
  # Manually define triad type labels
  triad_labels <- c("003","012","102","021D","021U","021C",
                    "111D","111U","030T","030C", "201",
                    "120D","120U","120C","210","300")
  
  # Create a data frame
  triad_census_df <- data.frame(
    triad = triad_labels,
    count = as.numeric(triad_census_result),
    network = net_name
  )
  
  # Calculate proportions and percentages
  triad_census_df <- triad_census_df %>%
    mutate(proportion = count / sum(count)) %>%
    mutate(percentage_label = percent(proportion, accuracy = 0.1))
  
  # Set triad column as an ordered factor to ensure plot order
  triad_census_df$triad <- factor(triad_census_df$triad, levels = triad_labels)
  
  # Create and return the plot
  ggplot(triad_census_df, aes(x = triad, y = proportion, fill = network)) +
    geom_bar(stat = "identity", alpha = 1) +
    geom_text(aes(label = percentage_label), 
              vjust = 0.5, hjust = -0.2, angle = 90, size = 2.5) +
    scale_fill_manual(values = c("cyan4")) +
    coord_cartesian(ylim = c(0, max(triad_census_df$proportion) * 1.1)) +  # Expand y-axis range
    theme_gray() +
    theme(legend.position = "none") +
    labs(title = paste("Triad Census Proportion:", net_name),
         x = "Triad Type", y = "Proportion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate and print plots for different networks
p1 <- plot_triad_census(net_all_WD, "net_all_WD")
p2 <- plot_triad_census(net_all, "net_all")
p3 <- plot_triad_census(net_moderate, "net_moderate")
p4 <- plot_triad_census(net_active, "net_active")

# Arrange plots in a grid layout
(p1 | p2)/(p3 | p4)

