# Load necessary libraries
library(igraph)

# Function to calculate and print network metrics
calculate_and_print_metrics <- function(network, network_name) {
  # Calculate diameter and vertex sequence along the diameter
  diameter_value <- diameter(network, directed = TRUE, weights = E(network)$weight)
  diam <- get_diameter(network, directed = TRUE, weights = E(network)$weight)
  
  # Calculate average distance
  avg_distance <- mean_distance(network, weights = E(network)$weight, directed = TRUE)
  
  # Calculate normalized average distance
  normalized_avg_distance <- avg_distance / diameter_value
  
  # Print results
  cat(paste("Network:", network_name, "\n"))
  cat(paste("Diameter:", diameter_value, "\n"))
  cat("Vertex sequence (diameter path):", diam, "\n")
  cat(paste("Mean distance:", avg_distance, "\n"))
  cat(paste("Normalized average distance:", normalized_avg_distance, "\n"))
  cat("\n")
}

# Function to visualize the network's diameter
plot_network_diameter <- function(net, layout, title) {
  diam <- get_diameter(net, directed = TRUE, weights = E(net)$weight)
  
  # Set vertex colors
  vcol <- rep("gray50", vcount(net))
  vcol[diam] <- "gold"
  
  # Set edge colors
  ecol <- rep("gray50", ecount(net))
  ecol[E(net, path = diam)] <- "orange"
  
  # Plot the network with specified layout and properties
  par(bg = "white")
  plot(net, layout = layout,
       main = title,
       vertex.color = vcol,
       vertex.frame.color = vcol,
       vertex.size = ifelse(vcol == "gold", 4, 2),
       vertex.label = NA,
       edge.color = ecol,
       edge.width = ifelse(ecol == "orange", 2, 0.1),
       edge.arrow.mode = 0,
       edge.arrow.size = 0.05)
}

# Calculate and print the edge density for each network
# Edge density for the 'net_all_WD' network
density_all_WD <- edge_density(net_all_WD)
cat("Edge Density of net_all_WD:", density_all_WD, "\n")
# Edge density for the 'net_all' network
density_all <- edge_density(net_all)
cat("Edge Density of net_all:", density_all, "\n")
# Edge density for the 'net_moderate' network
density_moderate <- edge_density(net_moderate)
cat("Edge Density of net_moderate:", density_moderate, "\n")
# Edge density for the 'net_active' network
density_active <- edge_density(net_active)
cat("Edge Density of net_active:", density_active, "\n")

# Calculate and print metrics for each network
calculate_and_print_metrics(net_all_WD, "net_all_WD")
calculate_and_print_metrics(net_all, "net_all")
calculate_and_print_metrics(net_moderate, "net_moderate")
calculate_and_print_metrics(net_active, "net_active")

# Visualize the network's diameter for each network
plot_network_diameter(net_all_WD, layout_all_WD, "Network All WD")
plot_network_diameter(net_all, layout_all, "Network All")
plot_network_diameter(net_moderate, layout_moderate, "Network Moderate")
plot_network_diameter(net_active, layout_active, "Network Active")
