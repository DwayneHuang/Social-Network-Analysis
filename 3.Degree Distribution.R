# Load necessary libraries
library(igraph)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(patchwork)

# Function to extract and normalize degree data from the network
get_data <- function(net, net_name, mode = "all") {
  degree_data <- degree(net, mode = mode, normalized = TRUE)  # Extract and normalize degree data
  data <- data.frame(type = net_name, value = degree_data)    # Create a data frame
  return(data)
}

# Function to plot the degree distribution histogram
print_degree_distribution <- function(net, net_name, mode = "all") {
  data <- get_data(net, net_name, mode = mode)                  # Get degree data
  unique_count <- length(unique(data$value))                    # Calculate the number of unique values for bins
  
  # Plot histogram
  p <- ggplot(data, aes(x = value, fill = type)) +
    geom_histogram(alpha = 1, bins = unique_count, fill = "deeppink3") +
    theme_gray() +
    theme(legend.position = "none") +
    labs(x = paste("Degree Distribution in", net_name, "(", mode, ")"),
         y = "Frequency")
  
  return(p)
}

# Generate and arrange degree distribution plots
p1 <- print_degree_distribution(net_all_WD, "Net_all_WD", mode = "all")
p2 <- print_degree_distribution(net_all_WD, "Net_all_WD", mode = "in")
p3 <- print_degree_distribution(net_all_WD, "Net_all_WD", mode = "out")
p4 <- print_degree_distribution(net_all, "Net_all", mode = "all")
p5 <- print_degree_distribution(net_all, "Net_all", mode = "in")
p6 <- print_degree_distribution(net_all, "Net_all", mode = "out")
p7 <- print_degree_distribution(net_moderate, "Net_moderate", mode = "all")
p8 <- print_degree_distribution(net_moderate, "Net_moderate", mode = "in")
p9 <- print_degree_distribution(net_moderate, "Net_moderate", mode = "out")
p10 <- print_degree_distribution(net_active, "Net_active", mode = "all")
p11 <- print_degree_distribution(net_active, "Net_active", mode = "in")
p12 <- print_degree_distribution(net_active, "Net_active", mode = "out")

# Combine the plots into a grid layout
(p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9) / (p10 | p11 | p12)
