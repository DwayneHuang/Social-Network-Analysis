# Load necessary libraries
library(igraph)

# Define a function to calculate and print network homophily and assortativity
calculate_and_print_homophily <- function(network, network_name) {
  party_homophily <- assortativity_nominal(network, types = as.integer(as.factor(V(network)$Party)), directed = TRUE, normalized = TRUE)
  state_homophily <- assortativity_nominal(network, types = as.integer(as.factor(V(network)$State)), directed = TRUE, normalized = TRUE)
  seniority_year_homophily <- assortativity_nominal(network, types = as.integer(as.factor(V(network)$Seniority.Year)), directed = TRUE, normalized = TRUE)
  seniority_homophily <- assortativity_nominal(network, types = as.integer(as.factor(V(network)$Seniority)), directed = TRUE, normalized = TRUE)
  house_homophily <- assortativity_nominal(network, types = as.integer(as.factor(V(network)$House)), directed = TRUE, normalized = TRUE)
  degree_assortativity <- assortativity_degree(network, directed = TRUE)
  
  cat(paste("Homophily of Party,", network_name, ":", party_homophily, "\n"))
  cat(paste("Homophily of State,", network_name, ":", state_homophily, "\n"))
  cat(paste("Homophily of Seniority Year,", network_name, ":", seniority_year_homophily, "\n"))
  cat(paste("Homophily of Seniority,", network_name, ":", seniority_homophily, "\n"))
  cat(paste("Homophily of House,", network_name, ":", house_homophily, "\n"))
  cat(paste("Degree assortativity,", network_name, ":", degree_assortativity, "\n"))
}

# Apply the function to all networks
calculate_and_print_homophily(net_all_WD, "Net_all_WD")
calculate_and_print_homophily(net_all, "Net_all")
calculate_and_print_homophily(net_moderate, "Net_moderate")
calculate_and_print_homophily(net_active, "Net_active")

# Calculate and print network reciprocity
cat(paste("Reciprocity of Net_all WD:", reciprocity(net_all_WD), "\n"))
cat(paste("Reciprocity of Net_all:", reciprocity(net_all), "\n"))
cat(paste("Reciprocity of Net_moderate:", reciprocity(net_moderate), "\n"))
cat(paste("Reciprocity of Net_active:", reciprocity(net_active), "\n"))

# Calculate and print network transitivity
cat(paste("Transitivity of Net_all WD:", transitivity(net_all_WD, type = "global"), "\n"))
cat(paste("Transitivity of Net_all:", transitivity(net_all, type = "global"), "\n"))
cat(paste("Transitivity of Net_moderate:", transitivity(net_moderate, type = "global"), "\n"))
cat(paste("Transitivity of Net_active:", transitivity(net_active, type = "global"), "\n"))
