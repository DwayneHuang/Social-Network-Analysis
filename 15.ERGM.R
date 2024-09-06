# Load necessary libraries
library(intergraph)  # For conversion between network and igraph objects
library(ergm)
library(latticeExtra)
library(gt)

# Convert igraph objects to network objects
ergm_net_all_WD <- asNetwork(net_all_WD)
ergm_net_all <- asNetwork(net_all)
ergm_net_moderate <- asNetwork(net_moderate)
ergm_net_active <- asNetwork(net_active)

# Display summaries of the converted network objects
summary(ergm_net_all_WD)
summary(ergm_net_all)
summary(ergm_net_moderate)
summary(ergm_net_active)

# Define ERGM models for different networks
set.seed(1012)

# Model for net_all
term.net_all <- summary(ergm_net_all ~ edges + 
                                       mutual + 
                                       nodefactor("Party") + 
                                       nodefactor("Seniority") +
                                       nodefactor("House") +
                                       nodematch("Party") +
                                       nodematch("State") +
                                       nodematch("Seniority", diff = TRUE) +
                                       nodematch("House"))

netmodel.01 <- ergm(ergm_net_all ~ edges +
                                   mutual + 
                                   nodefactor("Party") + 
                                   nodefactor("Seniority") +
                                   nodefactor("House") +
                                   nodematch("Party") +
                                   nodematch("State") +
                                   nodematch("Seniority", diff = TRUE) +
                                   nodematch("House"),
                                   verbose = TRUE)

summary(netmodel.01)
mcmc.diagnostics(netmodel.01)

netmodel.01.sim <- simulate(netmodel.01, nsim = 10)
sim.net_all <- rbind("obs.net_all" = term.net_all,
                     "sim mean.net_all" = round(colMeans(attr(netmodel.01.sim, "stats"))))

netmodel.01.gof <- gof(netmodel.01)
par(mfrow = c(1, 1))
plot(netmodel.01.gof)

# Model for net_moderate
term.net_moderate <- summary(ergm_net_moderate ~ edges + 
                                                 mutual + 
                                                 nodefactor("Party") + 
                                                 nodefactor("Seniority") +
                                                 nodefactor("House") +
                                                 nodematch("Party") +
                                                 nodematch("State") +
                                                 nodematch("Seniority", diff = TRUE) +
                                                 nodematch("House"))

netmodel.02 <- ergm(ergm_net_moderate ~ edges +
                                        mutual + 
                                        nodefactor("Party") + 
                                        nodefactor("Seniority") +
                                        nodefactor("House") +
                                        nodematch("Party") +
                                        nodematch("State") +
                                        nodematch("Seniority", diff = TRUE) +
                                        nodematch("House"),
                                        verbose = TRUE)

summary(netmodel.02)
mcmc.diagnostics(netmodel.02)

netmodel.02.sim <- simulate(netmodel.02, nsim = 10)
sim.net_moderate <- rbind("obs.net_moderate" = term.net_moderate,
                          "sim mean.net_moderate" = round(colMeans(attr(netmodel.02.sim, "stats"))))

netmodel.02.gof <- gof(netmodel.02)
plot(netmodel.02.gof)

# Model for net_active
term.net_active <- summary(ergm_net_active ~ edges + 
                                             mutual + 
                                             nodefactor("Party") + 
                                             nodefactor("Seniority") +
                                             nodefactor("House") +
                                             nodematch("Party") +
                                             nodematch("State") +
                                             nodematch("Seniority", diff = TRUE) +
                                             nodematch("House"))

netmodel.03 <- ergm(ergm_net_active ~ edges +
                                      mutual + 
                                      nodefactor("Party") + 
                                      nodefactor("Seniority") +
                                      nodefactor("House") +
                                      nodematch("Party") +
                                      nodematch("State") +
                                      nodematch("Seniority", diff = TRUE) +
                                      nodematch("House"),
                                      verbose = TRUE)

summary(netmodel.03)
mcmc.diagnostics(netmodel.03)

netmodel.03.sim <- simulate(netmodel.03, nsim = 10)
sim.net_active <- rbind("obs.net_active" = term.net_active,
                        "sim mean.net_active" = round(colMeans(attr(netmodel.03.sim, "stats"))))

netmodel.03.gof <- gof(netmodel.03)
plot(netmodel.03.gof)

# Combine and display results
term <- cbind(term.net_all, term.net_moderate, term.net_active)
as.data.frame(term) %>%
  gt(rownames_to_stub = TRUE) %>%
  gt_theme_nytimes()

sim <- t(rbind(sim.net_all, sim.net_moderate, sim.net_active))
as.data.frame(sim) %>%
  gt(rownames_to_stub = TRUE) %>%
  gt_theme_nytimes() %>%
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(1)),
    locations = cells_body(columns = c(3, 5))
  )
