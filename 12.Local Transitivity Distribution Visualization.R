# Load necessary libraries
library(igraph)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(patchwork)  # For arranging plots in a grid

# Function to get transitivity data for a single network
get_transitivity_data <- function(net, net_name) {
  # Extract local transitivity values
  transitivity_values <- transitivity(net, type = "local")
  
  # Create a data frame with the network name and transitivity values
  data <- data.frame(
    type = net_name,
    value = transitivity_values
  )
  
  return(data)  # Return the data frame
}

# Function to generate and print transitivity distribution plot
print_transitivity_distribution <- function(net, net_name) {
  # Get transitivity data
  data <- get_transitivity_data(net, net_name)
  
  # Generate histogram for local transitivity distribution
  p <- data %>%
    ggplot(aes(x = value, fill = type)) +
    geom_histogram(alpha = 1, position = 'identity', bins = 100, fill = "coral1") +  # Set appropriate number of bins
    theme_gray() +
    theme(legend.position = "none") +
    labs(x = paste(net_name, "Local Transitivity Distribution"),
         y = "Frequency")
  
  # Print the plot
  print(p)
}

# Generate and print plots for different networks
p1 <- print_transitivity_distribution(net_all_WD, "net_all_WD")
p2 <- print_transitivity_distribution(net_all, "net_all")
p3 <- print_transitivity_distribution(net_moderate, "net_moderate")
p4 <- print_transitivity_distribution(net_active, "net_active")

# Arrange plots in a grid layout
(p1 | p2)/(p3 | p4)
