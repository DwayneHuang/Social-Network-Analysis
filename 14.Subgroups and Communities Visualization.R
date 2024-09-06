# Load necessary libraries
library(igraph)
library(RColorBrewer)

# Function to plot cliques in the network
plot_cliques <- function(net, layout, title = "Cliques") {
  
  # Get all cliques and the largest cliques
  clq <- cliques(as.undirected(net))
  largest_clq <- largest_cliques(as.undirected(net))
  
  # Define vertex colors
  vcol <- rep("grey50", vcount(net))
  vcol[(V(net)$Party == "R")] <- "gray30"
  vcol[(V(net)$Party == "I")] <- "black"
  vcol[unlist(largest_clq)] <- "gold"
  
  # Define colors and shapes based on House
  colrs <- c("white", "gray0")
  shape <- c("circle", "square")
  
  # Define vertex sizes based on degree
  vertex_size <- degree(net, mode = "all", normalized = TRUE)
  
  # Plot network
  par(bg = "white")
  plot(net, layout = layout,
       main = title,
       vertex.color = vcol,
       vertex.frame.color = colrs[as.factor(V(net)$House)],
       vertex.shape = shape[as.factor(V(net)$House)],
       vertex.size = vertex_size * 25,
       vertex.label = NA,
       edge.color = "gray40",
       edge.width = 0.1,
       edge.arrow.mode = 0,
       edge.arrow.size = 0.05)
}

# Function to plot community detection results
plot_community_detection <- function(net, layout, method, title_prefix, print_info = TRUE) {
  set.seed(1012)
  
  # Define colors and shapes based on Party and House
  colrs1 <- c("darkslategrey", "lawngreen", "deeppink3")
  colP <- colrs1[as.factor(V(net)$Party)]
  colrs2 <- c("white", "gray0")
  shape <- c("circle", "pie", "square")
  
  # Perform community detection based on the chosen method
  if (method == "ceb") {
    community <- cluster_edge_betweenness(net)
    title <- paste(title_prefix, "Edge Betweenness")
  } else if (method == "clp") {
    community <- cluster_label_prop(net)
    title <- paste(title_prefix, "Label Propagation")
  } else if (method == "cfg") {
    community <- cluster_fast_greedy(as.undirected(net))
    title <- paste(title_prefix, "Fast Greedy")
  } else if (method == "crw") {
    community <- cluster_walktrap(net)
    title <- paste(title_prefix, "Random Walk")
  } else {
    stop("Invalid method. Use 'ceb', 'clp', 'cfg', or 'crw'.")
  }
  
  # Print community information if required
  if (print_info) {
    cat("Method:", method, "\n")
    cat("Modularity:", modularity(community), "\n")
    cat("Sizes of communities:\n")
    print(sizes(community))
    cat("Number of communities:", length(sizes(community)), "\n")
    cat("Crossing table:\n")
    print(table(igraph::crossing(community, net)))
  }
  
  # Plot community detection results
  par(bg = "gray70")
  plot(community, net, layout = layout,
       main = title,
       vertex.frame.color = colrs2[as.factor(V(net)$House)],
       vertex.shape = shape[as.factor(V(net)$Party)],
       vertex.size = 2,
       vertex.label = NA,
       edge.color = colP[ends(net, es = E(net), names = FALSE)[, 1]],
       edge.width = 0.1,
       edge.arrow.mode = 0,
       edge.arrow.size = 0.01)
}

# Function to normalize values using min-max normalization
normalize_min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Function to plot k-core decomposition
plot_kcore <- function(net, layout, title = "K-core") {
  kc <- coreness(net, mode = "all")
  
  # Define colors and shapes based on Party
  colrs2 <- c("white", "gray0")
  shape <- c("circle", "pie", "square")
  colrs <- colorRampPalette(brewer.pal(12, "Set3"))(max(kc))
  
  # Plot k-core decomposition
  par(bg = "gray70")
  par(bg = "white")
  plot(net, layout = layout,
       main = title,
       vertex.color = colrs[kc],
       vertex.frame.color = colrs[kc],
       vertex.shape = shape[as.factor(V(net)$Party)],
       vertex.size = normalize_min_max(kc) * 7,
       vertex.label = kc,
       vertex.label.cex = 0.5,
       edge.color = "gray30",
       edge.width = 0.1,
       edge.arrow.mode = 0,
       edge.arrow.size = 0.01)
}

# Plot cliques for different networks
plot_cliques(net_all_WD, layout_all_WD, "Network All WD - Cliques")
plot_cliques(net_all, layout_all, "Network All - Cliques")
plot_cliques(net_moderate, layout_moderate, "Network Moderate - Cliques")
plot_cliques(net_active, layout_active, "Network Active - Cliques")

# Plot community detection results for different networks and methods
plot_community_detection(net_all_WD, layout_all_WD, "ceb", "Network All WD - ")
plot_community_detection(net_all_WD, layout_all_WD, "clp", "Network All WD - ")
plot_community_detection(net_all_WD, layout_all_WD, "cfg", "Network All WD - ")
plot_community_detection(net_all_WD, layout_all_WD, "crw", "Network All WD - ")

plot_community_detection(net_all, layout_all, "ceb", "Network All - ")
plot_community_detection(net_all, layout_all, "clp", "Network All - ")
plot_community_detection(net_all, layout_all, "cfg", "Network All - ")
plot_community_detection(net_all, layout_all, "crw", "Network All - ")

plot_community_detection(net_moderate, layout_moderate, "ceb", "Network Moderate - ")
plot_community_detection(net_moderate, layout_moderate, "clp", "Network Moderate - ")
plot_community_detection(net_moderate, layout_moderate, "cfg", "Network Moderate - ")
plot_community_detection(net_moderate, layout_moderate, "crw", "Network Moderate - ")

plot_community_detection(net_active, layout_active, "ceb", "Network Active - ")
plot_community_detection(net_active, layout_active, "clp", "Network Active - ")
plot_community_detection(net_active, layout_active, "cfg", "Network Active - ")
plot_community_detection(net_active, layout_active, "crw", "Network Active - ")

# Plot k-core for different networks
plot_kcore(net_all_WD, layout_all_WD, "Network All WD - K-core")
plot_kcore(net_all, layout_all, "Network All - K-core")
plot_kcore(net_moderate, layout_moderate, "Network Moderate - K-core")
plot_kcore(net_active, layout_active, "Network Active - K-core")
