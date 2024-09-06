# Load necessary libraries
library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gt)
library(gtExtras)

# Define the network and its name (replace with your actual graph object)
net <- net_all_WD
net_name <- "Net_all_weighted"

net <- net_all
net_name <- "Net_all"

net <- net_moderate
net_name <- "Net_moderate"

net <- net_active
net_name <- "Net_active"

# Calculate centrality measures
V(net)$degree <- degree(net, mode = "all", normalized = TRUE)
V(net)$indegree <- degree(net, mode = "in", normalized = TRUE)
V(net)$outdegree <- degree(net, mode = "out", normalized = TRUE)
V(net)$betweenness <- betweenness(net, directed = TRUE, weights = E(net)$weight, normalized = TRUE)
V(net)$eigenvector <- eigen_centrality(net, directed = TRUE, weights = E(net)$weight)$vector
V(net)$closeness <- closeness(net, mode = "all", weights = E(net)$weight, normalized = TRUE)
V(net)$in_closeness <- closeness(net, mode = "in", weights = E(net)$weight, normalized = TRUE)
V(net)$out_closeness <- closeness(net, mode = "out", weights = E(net)$weight, normalized = TRUE)

# Calculate Hub and Authority scores
V(net)$hub_score <- hub_score(net, weights = E(net)$weight)$vector
V(net)$authority_score <- authority_score(net, weights = E(net)$weight)$vector

# Create a dataframe for node attributes including the House attribute
node_attr <- data.frame(
  ID = V(net)$name,
  Party = V(net)$Party,
  State = V(net)$State,
  Seniority = V(net)$Seniority,
  House = trimws(V(net)$House),  # Remove leading/trailing whitespace
  Degree = V(net)$degree,
  Indegree = V(net)$indegree,
  Outdegree = V(net)$outdegree,
  Betweenness = V(net)$betweenness,
  Eigenvector = V(net)$eigenvector,
  Closeness = V(net)$closeness,
  In_Closeness = V(net)$in_closeness,
  Out_Closeness = V(net)$out_closeness,
  Hub_Score = V(net)$hub_score,
  Authority_Score = V(net)$authority_score
)

# Calculate summary statistics by Party
party_stats <- node_attr %>%
  group_by(Party) %>%
  summarise(
    Avg_Degree = round(mean(Degree, na.rm = TRUE), 3),
    Avg_Indegree = round(mean(Indegree, na.rm = TRUE), 3),
    Avg_Outdegree = round(mean(Outdegree, na.rm = TRUE), 3),
    Avg_Betweenness = round(mean(Betweenness, na.rm = TRUE), 3),
    Avg_Eigenvector = round(mean(Eigenvector, na.rm = TRUE), 3),
    Avg_Closeness = round(mean(Closeness, na.rm = TRUE), 3),
    Avg_In_Closeness = round(mean(In_Closeness, na.rm = TRUE), 3),
    Avg_Out_Closeness = round(mean(Out_Closeness, na.rm = TRUE), 3),
    Avg_Hub_Score = round(mean(Hub_Score, na.rm = TRUE), 3),
    Avg_Authority_Score = round(mean(Authority_Score, na.rm = TRUE), 3)
  )

# Calculate summary statistics by Seniority
seniority_stats <- node_attr %>%
  group_by(Seniority) %>%
  summarise(
    Avg_Degree = round(mean(Degree, na.rm = TRUE), 3),
    Avg_Indegree = round(mean(Indegree, na.rm = TRUE), 3),
    Avg_Outdegree = round(mean(Outdegree, na.rm = TRUE), 3),
    Avg_Betweenness = round(mean(Betweenness, na.rm = TRUE), 3),
    Avg_Eigenvector = round(mean(Eigenvector, na.rm = TRUE), 3),
    Avg_Closeness = round(mean(Closeness, na.rm = TRUE), 3),
    Avg_In_Closeness = round(mean(In_Closeness, na.rm = TRUE), 3),
    Avg_Out_Closeness = round(mean(Out_Closeness, na.rm = TRUE), 3),
    Avg_Hub_Score = round(mean(Hub_Score, na.rm = TRUE), 3),
    Avg_Authority_Score = round(mean(Authority_Score, na.rm = TRUE), 3)
  )

# Calculate summary statistics by House
house_stats <- node_attr %>%
  group_by(House) %>%
  summarise(
    Avg_Degree = round(mean(Degree, na.rm = TRUE), 3),
    Avg_Indegree = round(mean(Indegree, na.rm = TRUE), 3),
    Avg_Outdegree = round(mean(Outdegree, na.rm = TRUE), 3),
    Avg_Betweenness = round(mean(Betweenness, na.rm = TRUE), 3),
    Avg_Eigenvector = round(mean(Eigenvector, na.rm = TRUE), 3),
    Avg_Closeness = round(mean(Closeness, na.rm = TRUE), 3),
    Avg_In_Closeness = round(mean(In_Closeness, na.rm = TRUE), 3),
    Avg_Out_Closeness = round(mean(Out_Closeness, na.rm = TRUE), 3),
    Avg_Hub_Score = round(mean(Hub_Score, na.rm = TRUE), 3),
    Avg_Authority_Score = round(mean(Authority_Score, na.rm = TRUE), 3)
  )

# Adjust the order of party_stats and seniority_stats
party_stats <- party_stats[c(1, 3, 2), ]
seniority_stats <- seniority_stats[c(1, 3, 2), ]

# Output descriptive statistics with formatting
party_stats %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = net_name) %>%
  gt_highlight_rows(
    target_col = c(6, 10, 11),
    bold_target_only = TRUE,
    fill = "white",
    font_color = "#000051"
  )

seniority_stats %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = net_name)

house_stats %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = net_name) %>%
  gt_highlight_rows(
    target_col = c(6, 10, 11),
    bold_target_only = TRUE,
    fill = "white",
    font_color = "#000051"
  )
