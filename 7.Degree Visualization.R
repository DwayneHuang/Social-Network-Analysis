# Load necessary packages
library(igraph)

# Define a function to plot the network with customizable settings
plot_network <- function(net, layout, mode = "all", title = "Network Plot") {
  # Define colors based on Party affiliation
  colrs1 <- c("darkslategrey", "lawngreen", "deeppink3")
  colP <- colrs1[as.factor(V(net)$Party)]
  
  # Define frame colors based on House affiliation
  colrs2 <- c("white", "gray0")
  
  # Set scale for vertex size
  scale <- 30
  
  # Determine vertex size based on the selected degree mode
  if (mode == "all") {
    vertex_size <- degree(net, mode = "all", normalized = TRUE) * scale
  } else if (mode == "in") {
    vertex_size <- degree(net, mode = "in", normalized = TRUE) * scale
  } else if (mode == "out") {
    vertex_size <- degree(net, mode = "out", normalized = TRUE) * scale
  } else {
    stop("Mode must be one of 'all', 'in', or 'out'")
  }
  
  # Plot the network
  plot(net, layout = layout,
       main = title,
       vertex.color = colP,                                 # Vertex color based on Party
       vertex.frame.color = colrs2[as.factor(V(net)$House)], # Vertex frame color based on House
       vertex.size = vertex_size,                            # Vertex size based on degree
       vertex.label = NA,
       edge.color = colP[ends(net, es = E(net), names = FALSE)[, 1]], # Edge color based on Party
       edge.width = 0.1,
       edge.arrow.size = 0.05)
}

# Set background color
par(bg = "white") 

# Plot networks with different background colors for each group of plots
par(bg = "grey60")
plot_network(net_all_WD, layout_all_WD, mode = "all", title = "All WD Network - All Degree")
plot_network(net_all_WD, layout_all_WD, mode = "in", title = "All WD Network - In Degree")
plot_network(net_all_WD, layout_all_WD, mode = "out", title = "All WD Network - Out Degree")

par(bg = "grey65")
plot_network(net_all, layout_all, mode = "all", title = "All Network - All Degree")
plot_network(net_all, layout_all, mode = "in", title = "All Network - In Degree")
plot_network(net_all, layout_all, mode = "out", title = "All Network - Out Degree")

par(bg = "grey70")
plot_network(net_moderate, layout_moderate, mode = "all", title = "Moderate Network - All Degree")
plot_network(net_moderate, layout_moderate, mode = "in", title = "Moderate Network - In Degree")
plot_network(net_moderate, layout_moderate, mode = "out", title = "Moderate Network - Out Degree")

par(bg = "grey80")
plot_network(net_active, layout_active, mode = "all", title = "Active Network - All Degrees")
plot_network(net_active, layout_active, mode = "in", title = "Active Network - In Degrees")
plot_network(net_active, layout_active, mode = "out", title = "Active Network - Out Degrees")
