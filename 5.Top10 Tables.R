# Load necessary libraries
library(gt)
library(dplyr)
library(gtExtras)
library(glue)

# Function to calculate network measures
calculate_measures <- function(net) {
  measures <- data.frame(
    ID = V(net)$name,
    Name = V(net)$Name,
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

# Function to get top 10 vertices for a given centrality measure and format as a table
get_top10 <- function(measures, measure) {
  # Sort and extract top 10
  sorted_measures <- measures[order(-measures[[measure]]), ]
  top10 <- sorted_measures[1:10, c("ID", "Name", "Party", "House", "Seniority", measure)]
  # Round the centrality values to 3 decimal places
  top10[[measure]] <- round(top10[[measure]], 3)
  # Set row colors based on Party and House attributes
  top10$color <- with(top10, ifelse(Party == "D" & House == "Representatives", "lightskyblue3",
                                    ifelse(Party == "D" & House == "Senate", "lightskyblue1",
                                           ifelse(Party == "R" & House == "Representatives", "lightsalmon", "bisque1"))))
  colnames(top10)[6] <- "Centrality"  # Rename centrality column
  return(top10)
}

# Function to create and print combined table for each network and measure
generate_and_print_combined_table <- function(net, net_name) {
  measures <- calculate_measures(net)
  centrality_measures <- c("degr_all", "degr_in", "degr_out", "btwn", "eigen", "cls_all", "cls_in", "cls_out", "hs", "as")
  
  # Initialize an empty dataframe to store top 10 for all measures
  combined_df <- data.frame()
  
  # Get top 10 for each measure and combine into one dataframe
  for (measure in centrality_measures) {
    top10 <- get_top10(measures, measure)
    top10 <- rbind(names(top10), top10)
    combined_df <- rbind(combined_df, top10)
  }
  
  # Create a gt table object
  table <- combined_df[,c(1,2,5,6)] %>%
    gt() %>% 
    gt_theme_excel()  # Use Excel-like theme
  
  # Names to highlight in bold
  highlight_names <- c("Kevin McCarthy", "Nancy Pelosi", "C. Scott Franklin")
  
  # Set row color and bold specific names
  for (i in 1:nrow(combined_df)) {
    table <- table %>%
      tab_style(
        style = cell_fill(color = ifelse(combined_df$color[i] == "color", "white", combined_df$color[i])),  # Set row color
        locations = cells_body(rows = i)
      )
    if (combined_df$Name[i] %in% highlight_names) {
      table <- table %>%
        tab_style(
          style = cell_text(weight = "bold"),  # Set text to bold for specific names
          locations = cells_body(rows = i)
        )
    }
  }
  
  # Print the table and add a title
  print(table)
  cat(paste("\nCombined Top 10 Centrality Measures for", net_name, "\n\n"))
  
  # Save the table as a PNG
  #gtsave(table, glue("/path/gt_table_{net_name}.png"))
} 

# Generate tables for different networks
generate_and_print_combined_table(net_all_WD, "net_all_WD")
generate_and_print_combined_table(net_all, "net_all")
generate_and_print_combined_table(net_moderate, "net_moderate")
generate_and_print_combined_table(net_active, "net_active")
