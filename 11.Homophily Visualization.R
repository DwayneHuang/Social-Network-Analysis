# Load necessary libraries
library(igraph)
library(RColorBrewer)
library(grDevices)

# Define a function to plot network attributes with different color schemes
plot_network_attributes <- function(net, layout, title = "Network Plot") {
  
  # Party-based colors
  pa <- as.numeric(as.factor(V(net)$Party))
  colrpa <- c("darkslategrey", "lawngreen", "deeppink3")
  vertex_colors_pa <- colrpa[pa]
  
  # State-based colors
  se <- as.numeric(as.factor(V(net)$State))
  colrse <- colorRampPalette(brewer.pal(12, "Set3"))(max(se))
  vertex_colors_se <- colrse[se]
  
  # Seniority-based colors
  sy <- ifelse(V(net)$Seniority == "High", 3, 
               ifelse(V(net)$Seniority == "Medium", 2, 1))
  colrsy <- adjustcolor(c("gray100", "gray40", "black"), alpha.f = 0.5)
  vertex_colors_sy <- colrsy[sy]
  
  # House-based colors
  ho <- as.numeric(as.factor(V(net)$House))
  colrho <- c("cyan4", "coral1")
  vertex_colors_ho <- colrho[ho]
  
  # Set plot background to white
  par(bg = "white")
  
  # Plot network colored by Party
  plot(net, layout = layout,
       main = paste(title, "- Party"),
       vertex.color = vertex_colors_pa,
       vertex.frame.color = vertex_colors_pa,
       vertex.size = 3,
       vertex.label = NA,
       vertex.label.cex = 0.5,
       vertex.label.color = "white",
       edge.color = "black",
       edge.width = 0.05,
       edge.arrow.mode = 0,
       edge.arrow.size = 0.01)
  
  # Plot network colored by State
  plot(net, layout = layout,
       main = paste(title, "- State"),
       vertex.color = vertex_colors_se,
       vertex.frame.color = vertex_colors_se,
       vertex.size = 3,
       vertex.label = V(net)$State,
       vertex.label.cex = 0.5,
       edge.color = "black",
       edge.width = 0.05,
       edge.arrow.mode = 0,
       edge.arrow.size = 0.01)
  
  # Set plot background to gray
  par(bg = "gray70")
  
  # Plot network colored by Seniority
  plot(net, layout = layout,
       main = paste(title, "- Seniority"),
       vertex.color = vertex_colors_sy,
       vertex.frame.color = vertex_colors_sy,
       vertex.size = 3,
       vertex.label = NA,
       vertex.label.cex = 0.5,
       edge.color = "black",
       edge.width = 0.05,
       edge.arrow.mode = 0,
       edge.arrow.size = 0.01)
  
  # Reset plot background to white
  par(bg = "white")
  
  # Plot network colored by House
  plot(net, layout = layout,
       main = paste(title, "- House"),
       vertex.color = vertex_colors_ho,
       vertex.frame.color = vertex_colors_ho,
       vertex.size = 3,
       vertex.label = NA,
       vertex.label.cex = 0.5,
       edge.color = "black",
       edge.width = 0.05,
       edge.arrow.mode = 0,
       edge.arrow.size = 0.01)
}

# Apply the function to different networks and layouts
plot_network_attributes(net_all_WD, layout_all_WD, "Net All WD")
plot_network_attributes(net_all, layout_all, "Net All")
plot_network_attributes(net_moderate, layout_moderate, "Net Moderate")
plot_network_attributes(net_active, layout_active, "Net Active")
