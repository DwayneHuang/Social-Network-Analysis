# Load necessary libraries
library(igraph)
library(corrplot)
library(knitr)

# Function to calculate network measures
calculate_measures <- function(net) {
  measures <- data.frame(
    vertex = V(net)$name,
    name = V(net)$Name,
    Party = V(net)$Party,
    House = V(net)$House,
    Seniority.Year = V(net)$Seniority.Year,
    Seniority = V(net)$Seniority,
    State = V(net)$State,
    degr_all = degree(net, mode="all", normalized = TRUE),
    degr_in = degree(net, mode="in", normalized = TRUE),
    degr_out = degree(net, mode="out", normalized = TRUE),
    btwn = betweenness(net, directed = TRUE, weights = E(net)$weight, normalized = TRUE),
    eigen = eigen_centrality(net, directed = TRUE, weights = E(net)$weight)$vector,
    cls_all = closeness(net, mode="all", weights = E(net)$weight, normalized = TRUE),
    cls_in = closeness(net, mode="in", weights = E(net)$weight, normalized = TRUE),
    cls_out = closeness(net, mode="out", weights = E(net)$weight, normalized = TRUE),
    hs = hub_score(net, weights = E(net)$weight)$vector,
    as = authority_score(net, weights = E(net)$weight)$vector
  )
  return(measures)
}

# Function to get top 10 vertices by a specific centrality measure
get_top10 <- function(measures, measure) {
  # Sort and extract the top 10
  sorted_measures <- measures[order(-measures[[measure]]), ]
  top10 <- sorted_measures[1:10, c("vertex", "name", "Party", "House", "Seniority", measure)]
  # Round the centrality measure to 3 decimal places
  top10[[measure]] <- round(top10[[measure]], 3)
  # Print as a markdown table
  kable(top10, format = "markdown", col.names = c("Vertex", "Name", "Party", "House", "Seniority", "Centrality"))
}

# Function to print top 10 vertices for a specific centrality measure
print_top10 <- function(measures, measure, net_name) {
  cat("\nTop 10 vertices for", measure, "in", net_name, ":\n")
  print(get_top10(measures, measure))
}

# Function to calculate and return centralization for a given measure
get_centralization <- function(net, measure, mode="all") {
  # Compute centralization based on the specified measure
  if (measure == "btwn") {
    centr_value <- centr_betw(net, directed = TRUE, normalized = TRUE)$centralization
  } else if (measure == "eigen") {
    centr_value <- centr_eigen(net, directed = TRUE, normalized = TRUE)$centralization
  } else if (grepl("degr", measure)) {
    centr_value <- centr_degree(net, mode = mode, normalized = TRUE)$centralization
  } else if (grepl("cls", measure)) {
    centr_value <- centr_clo(net, mode = mode, normalized = TRUE)$centralization
  } else {
    centr_value <- NA
    warning("Unknown measure: ", measure)
  }
  return(centr_value)
}

# Function to analyze and print results for a network
analyze_network <- function(net, net_name) {
  measures <- calculate_measures(net)
  
  # Degree centrality
  print_top10(measures, "degr_all", net_name)
  cat("\nDegree centralization (all),", net_name, ":", get_centralization(net, "degr_all", mode = "all"), "\n")
  cat("--------------------------------------------")
  
  # In-Degree centrality
  print_top10(measures, "degr_in", net_name)
  cat("\nIn-Degree centralization,", net_name, ":", get_centralization(net, "degr_in", mode = "in"), "\n")
  cat("--------------------------------------------")
  
  # Out-Degree centrality
  print_top10(measures, "degr_out", net_name)
  cat("\nOut-Degree centralization,", net_name, ":", get_centralization(net, "degr_out", mode = "out"), "\n")
  cat("--------------------------------------------")
  
  # Betweenness centrality
  print_top10(measures, "btwn", net_name)
  cat("\nBetweenness centralization,", net_name, ":", get_centralization(net, "btwn"), "\n")
  cat("--------------------------------------------")
  
  # Eigenvector centrality
  print_top10(measures, "eigen", net_name)
  cat("\nEigenvector centralization,", net_name, ":", get_centralization(net, "eigen"), "\n")
  cat("--------------------------------------------")
  
  # Closeness centrality (all)
  print_top10(measures, "cls_all", net_name)
  cat("\nCloseness centralization (all),", net_name, ":", get_centralization(net, "cls_all", mode = "all"), "\n")
  cat("--------------------------------------------")
  
  # Closeness centrality (in)
  print_top10(measures, "cls_in", net_name)
  cat("\nCloseness centralization (in),", net_name, ":", get_centralization(net, "cls_in", mode = "in"), "\n")
  cat("--------------------------------------------")
  
  # Closeness centrality (out)
  print_top10(measures, "cls_out", net_name)
  cat("\nCloseness centralization (out),", net_name, ":", get_centralization(net, "cls_out", mode = "out"), "\n")
  cat("--------------------------------------------")
  
  # Hubs
  print_top10(measures, "hs", net_name)
  cat("--------------------------------------------")
  
  # Authorities
  print_top10(measures, "as", net_name)
  cat("--------------------------------------------")
  
  # Correlation of various centrality measures
  cor_matrix <- cor(measures[8:17], use = "pairwise.complete.obs")
  corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.75, 
           mar = c(0, 0, 3, 0), title = net_name)
}

par(mfrow = c(1, 1))

# Analyze networks
analyze_network(net_all_WD, "net_all_WD")
analyze_network(net_all, "net_all")
analyze_network(net_moderate, "net_moderate")
analyze_network(net_active, "net_active")
