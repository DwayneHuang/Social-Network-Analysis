rm(list = ls())
library(igraph)
library(MASS)

# Load edge list
edge_list_url <- "https://raw.githubusercontent.com/DwayneHuang/Social-Network-Analysis/main/congress.edgelist"
edges <- read.table(edge_list_url, header = FALSE, stringsAsFactors = FALSE)

# Remove the last character from each value in the 4th column
edges$V4 <- sapply(edges$V4, function(x) substring(x, first = 1, last = nchar(x) - 1))

# Select relevant columns and rename them
edge_list <- edges[, c(1:2, 4)]
colnames(edge_list) <- c("from", "to", "weight")
edge_list$weight <- as.numeric(edge_list$weight)

# Check the number of rows and unique edges
nrow(edge_list)
nrow(unique(edge_list[,c("from", "to")]))

# Load node attributes
node_attr_url <- "https://raw.githubusercontent.com/DwayneHuang/Social-Network-Analysis/main/node_attributes_reorder.csv"
node_attr <- read.csv(node_attr_url, stringsAsFactors = FALSE)

# Combine First.Name and Last.Name into a single column 'Name'
node_attr$Name <- paste(node_attr$First.Name, node_attr$Last.Name, sep = " ")

# Add an ID column starting from 0
node_attr$ID <- 0:(nrow(node_attr) - 1)

# Correct the 'House' attribute
node_attr$House[node_attr$House != "Senate"] <- "Representatives"

# Create igraph object
network <- graph_from_data_frame(d=edge_list, vertices=node_attr[,c("ID","Name","Handle","State","Party","Seniority.Year","House")], directed=TRUE) 

# Analyze seniority distribution
seniority <- V(network)$Seniority.Year 
hist(seniority, breaks=min(seniority):max(seniority))
table(seniority)

# Calculate quartiles for seniority
quartiles <- quantile(seniority, probs = c(0.25, 0.50, 0.75))

# Categorize seniority into three levels based on quartiles
seniority_levels <- cut(seniority, 
                        breaks = c(-Inf, quartiles[1], quartiles[3], Inf), 
                        labels = c("High", "Medium", "Low"),
                        right = FALSE)
table(seniority_levels)

# Assign seniority levels to the network vertices
V(network)$Seniority <- as.character(seniority_levels)

# Threshold Selection
all_weights <- edge_list$weight

# Plot histogram of edge weights with log-normal fit
hist(all_weights, breaks = 150, probability = TRUE, main = "Histogram of Edge Weights with Log-normal Fit", 
     xlab = "Connection weight", ylab = "Probability density", col = "darkslategrey", ylim = c(0, 200))

# Fit a log-normal distribution to the edge weights
fit1 <- fitdistr(all_weights, "lognormal")

# Extract parameters
mean1 <- fit1$estimate[1]
sd1 <- fit1$estimate[2]

# Generate x-values for the fitted curve
x_vals <- seq(min(all_weights), max(all_weights), length.out = 1000)

# Calculate y-values for the fitted log-normal distribution
y_vals <- dlnorm(x_vals, meanlog = mean1, sdlog = sd1)

# Add the fitted curve to the histogram
lines(x_vals, y_vals, col = "red", lwd = 2)

# Log-transform edge weights and plot the histogram with normal fit
log_weights <- log(all_weights)
hist(log_weights, breaks = 150, probability = TRUE, main = "Histogram of Edge Weights with Normal Fit", 
     xlab = "Log(Connection weight)", ylab = "Probability density",col = "darkslategrey", ylim = c(0, 1))

# Fit a normal distribution to the log-transformed edge weights
fit2 <- fitdistr(log_weights, "normal")

# Extract parameters
mean2 <- fit2$estimate[1]
sd2 <- fit2$estimate[2]

# Generate x-values for the fitted curve
x_vals <- seq(min(log_weights), max(log_weights), length.out = 1000)

# Calculate y-values for the fitted normal distribution
y_vals <- dnorm(x_vals, mean = mean2, sd = sd2)

# Add the fitted curve to the histogram
lines(x_vals, y_vals, col = "red", lwd = 2)

# Draw vertical lines at mean and ±1 SD
abline(v = mean2, col = "blue", lwd = 2, lty = 2) # At the mean
abline(v = mean2 - sd2, col = "green", lwd = 2, lty = 2) # At mean - 1 SD
abline(v = mean2 + sd2, col = "green", lwd = 2, lty = 2) # At mean + 1 SD

# Add a legend
legend("topright", inset = c(0, 0), legend = c("Fitted Normal Distribution", "Mean", "Mean ± 1 SD"),
       col = c("red", "blue", "green"), lwd = 2, lty = 2, cex = 1, bty = "n")

# Define thresholds for edge weights based on the fitted normal distribution
threshold1 <- exp(mean2-sd2)
threshold2 <- exp(mean2+sd2)

# Network Segmentation
net_all_WD <- network
net_all <- network
net_moderate <- delete_edges(net_all, E(net_all)[weight<threshold1])
net_active <- delete_edges(net_all, E(net_all)[weight<threshold2])

# Identify and remove isolated nodes in the segmented networks
isolated_nodes_moderate <- V(net_moderate)[degree(net_moderate) == 0]
isolated_nodes_active <- V(net_active)[degree(net_active) == 0]
node_attr[isolated_nodes_active, c(8,1,4:7)]
net_active <- delete_vertices(net_active, isolated_nodes_active)

# Calculate the proportion of edges within the threshold range
(length(E(net_all)[weight>threshold1 & weight<threshold2]))/length(E(net_all))

# View summary information for each network
print(summary(net_all_WD))
print(summary(net_all))
print(summary(net_moderate))
print(summary(net_active))

# Remove edge weights from networks
net_all <- delete_edge_attr(net_all, "weight")
net_moderate <- delete_edge_attr(net_moderate, "weight")
net_active <- delete_edge_attr(net_active, "weight")

# View summary information for each network after weight removal
print(summary(net_all))
print(summary(net_moderate))
print(summary(net_active))

# Sum of Party, State, Seniority, House
# Set plotting area to a single panel
par(mfrow = c(1, 1))

# Barplot of Party Distribution by State
tab_party_state <- table(V(net_all)$Party, V(net_all)$State, dnn = c("Party", "State"))
tab_party_state_df <- as.data.frame(tab_party_state)
agg_party_state <- aggregate(Freq ~ State, data = tab_party_state_df, sum)
sorted_agg_party_state <- agg_party_state[order(-agg_party_state$Freq), ]
reordered_tab_party_state <- t(t(tab_party_state)[sorted_agg_party_state$State, ])
barplot(reordered_tab_party_state, beside = FALSE, 
        legend = rownames(tab_party_state), 
        args.legend = list(x = "topright", bty = "n"),
        col = c("darkslategrey", "lawngreen", "deeppink3"), 
        ylab = "Number of Members", las = 2, 
        main = "Party Distribution by State")

# Barplot of House Distribution by State
tab_house_state <- table(V(net_all)$House, V(net_all)$State, dnn = c("House", "State"))
tab_house_state_df <- as.data.frame(tab_house_state)
agg_house_state <- aggregate(Freq ~ State, data = tab_house_state_df, sum)
sorted_agg_house_state <- agg_house_state[order(-agg_house_state$Freq), ]
reordered_tab_house_state <- t(t(tab_house_state)[sorted_agg_house_state$State, ])
barplot(reordered_tab_house_state, beside = FALSE, 
        legend = rownames(tab_house_state), 
        args.legend = list(x = "topright", inset = c(-0.2, 0), bty = "n"),
        col = c("cyan4", "coral1"), 
        ylab = "Number of Members", las = 2, 
        main = "House Distribution by State")

# Barplot of Party Distribution by Seniority
tab_party_seniority <- table(V(net_all)$Party, V(net_all)$Seniority.Year, dnn = c("Party", "Seniority.Year"))
barplot(tab_party_seniority, beside = FALSE, 
        legend = rownames(tab_party_seniority), 
        args.legend = list(x = "topleft", bty = "n"),
        col = c("darkslategrey", "lawngreen", "deeppink3"), 
        ylab = "Number of Members", las = 2, 
        main = "Party Distribution by Seniority")

# Barplot of House Distribution by Seniority
tab_house_seniority <- table(V(net_all)$House, V(net_all)$Seniority.Year, dnn = c("House", "Seniority.Year"))
barplot(tab_house_seniority, beside = FALSE, 
        legend = rownames(tab_house_seniority), 
        args.legend = list(x = "topleft", bty = "n"),
        col = c("cyan4", "coral1"), 
        ylab = "Number of Members", las = 2, 
        main = "House Distribution by Seniority")

# Barplot of Party Distribution by House
tab_party_house <- table(V(net_all_WD)$Party, V(net_all)$House, dnn = c("Party", "House"))
barplot(tab_party_house, col = c("darkslategrey", "lawngreen", "deeppink3"), 
        legend = rownames(tab_party_house), 
        args.legend = list(x = "topright", bty = "n"), 
        ylab = "Number of Members", 
        main = "Party Distribution by House")

# Set seed for reproducibility and compute layouts
set.seed(2024)
layout_all_WD <- layout_with_fr(net_all_WD)
layout_all <- layout_with_fr(net_all)
layout_moderate <- layout_with_fr(net_moderate)
layout_active <- layout_with_fr(net_active)
