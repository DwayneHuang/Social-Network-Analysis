# Load necessary libraries
library(igraph)
library(gt)

# Define a function to process neighbor attributes for a specific node
process_node_neighbors <- function(net, net_name, central_vertex_name) {
  # Get neighboring nodes in different modes
  neigh_nodes_all <- neighbors(net, V(net)[Name == central_vertex_name], mode = "all")
  neigh_nodes_in <- neighbors(net, V(net)[Name == central_vertex_name], mode = "in")
  neigh_nodes_out <- neighbors(net, V(net)[Name == central_vertex_name], mode = "out")
  
  # Ensure all levels of each attribute are included even if count is 0
  party_levels <- unique(V(net)$Party)
  seniority_levels <- unique(V(net)$Seniority)
  house_levels <- unique(V(net)$House)
  
  # Bind tables of attributes for different modes, ensuring all levels are present
  x <- rbind(
    table(factor(neigh_nodes_in$Party, levels = party_levels)),
    table(factor(neigh_nodes_out$Party, levels = party_levels)),
    table(factor(neigh_nodes_all$Party, levels = party_levels))
  )
  
  y <- rbind(
    table(factor(neigh_nodes_in$Seniority, levels = seniority_levels)),
    table(factor(neigh_nodes_out$Seniority, levels = seniority_levels)),
    table(factor(neigh_nodes_all$Seniority, levels = seniority_levels))
  )
  
  z <- rbind(
    table(factor(neigh_nodes_in$House, levels = house_levels)),
    table(factor(neigh_nodes_out$House, levels = house_levels)),
    table(factor(neigh_nodes_all$House, levels = house_levels))
  )
  
  # Combine the attribute tables and create a data frame
  table <- data.frame(cbind(x, y[, c(2, 1, 3)], z[, c(2, 1)]))
  
  # Add direction labels and convert to a data frame column
  table <- cbind(Direction = c("in", "out", "all"), table)
  
  # Generate the table and add a title and subtitle
  table %>%
    gt() %>%
    gt_theme_nytimes() %>%
    tab_header(
      title = net_name,
      subtitle = paste(central_vertex_name, "- Number of Neighbors:", length(neigh_nodes_all))
    )
}

# Process nodes in different networks
process_node_neighbors(net_all_WD, "Net_all_WD", "Kevin McCarthy")
process_node_neighbors(net_all_WD, "Net_all_WD", "C. Scott Franklin")
process_node_neighbors(net_all_WD, "Net_all_WD", "Nancy Pelosi")

process_node_neighbors(net_all, "Net_all", "Kevin McCarthy")
process_node_neighbors(net_all, "Net_all", "C. Scott Franklin")
process_node_neighbors(net_all, "Net_all", "Nancy Pelosi")

process_node_neighbors(net_moderate, "Net_moderate", "Kevin McCarthy")
process_node_neighbors(net_moderate, "Net_moderate", "C. Scott Franklin")
process_node_neighbors(net_moderate, "Net_moderate", "Nancy Pelosi")

process_node_neighbors(net_active, "Net_active", "Kevin McCarthy")
process_node_neighbors(net_active, "Net_active", "C. Scott Franklin")
process_node_neighbors(net_active, "Net_active", "Nancy Pelosi")
