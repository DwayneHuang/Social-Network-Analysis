# Load necessary library
library(igraph)

# Define function to plot network scores
plot_network_scores <- function(net, layout, score_type, title_prefix) {
  # Define colors based on Party attribute
  colrs1 <- c("cyan4", "lawngreen", "coral1")  
  colP <- colrs1[as.factor(V(net)$Party)]
  
  # Define frame colors based on House attribute
  colrs2 <- c("white", "gray0")  
  
  # Scaling factor for vertex size
  scale <- 10
  
  # Calculate hub or authority scores based on score_type
  if (score_type == "hs") {
    scores <- hub_score(net, weights = E(net)$weight)$vector
    title <- paste(title_prefix, "Hubs")
  } else if (score_type == "as") {
    scores <- authority_score(net, weights = E(net)$weight)$vector
    title <- paste(title_prefix, "Authorities")
  } else {
    stop("Invalid score_type. Use 'hs' for hub scores or 'as' for authority scores.")
  }
  
  # Plot the network
  par(bg = "white")
  plot(net, layout = layout,
       main = title,
       vertex.color = colP,  # Vertex color based on Party
       vertex.frame.color = colrs2[as.factor(V(net)$House)],  # Frame color based on House
       vertex.size = scores * scale, 
       vertex.label = NA,
       edge.color = "gray30",
       edge.width = 0.05,
       edge.arrow.size = 0.01)
}

# Example function calls for different networks and score types
plot_network_scores(net_all_WD, layout_all_WD, "hs", "Network All WD - ")
plot_network_scores(net_all_WD, layout_all_WD, "as", "Network All WD - ")

plot_network_scores(net_all, layout_all, "hs", "Network All - ")
plot_network_scores(net_all, layout_all, "as", "Network All - ")

plot_network_scores(net_moderate, layout_moderate, "hs", "Network Moderate - ")
plot_network_scores(net_moderate, layout_moderate, "as", "Network Moderate - ")

plot_network_scores(net_active, layout_active, "hs", "Network Active - ")
plot_network_scores(net_active, layout_active, "as", "Network Active - ")
