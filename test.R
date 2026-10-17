# Define your initial nodes (including the new 'sociability' column)
node_preset_data <- data.frame(
  id = c("DSS", "LouisT", "MemberA", "MemberB"),
  label = c("DSS", "LouisT", "MemberA", "MemberB"),
  sociability = c(8, 5, 2, 7)
)

# Define your initial edges (connecting them with starting weights/widths)
edge_preset_data <- data.frame(
  from = c("DSS", "LouisT", "MemberA"),
  to =   c("LouisT", "MemberB", "MemberB"),
  width = c(5, 3, 4)
)

g <- graph_from_data_frame(
  d = edge_preset_data, 
  directed = FALSE,
  vertices = node_preset_data
)

dist_matrix <- distances(g)

library(igraph)

participation_rate <- 0.7


run_ecosystem_sim <- function(nodes_df, edges_df, steps = 10) {
  
 for(t in 1:steps) {
   
  active_nodes <- sample(nodes_df$id, size = ceiling(legnth(nodes_df$id)*participation_rate))
  remaining <- active_nodes
  
  while(length(remaining) > 1) {
    
    node_i <- sample(remaining, 1)
    
    # Connects to anyone for now? Allowing the delta parameter to allow connections or not for far away nodes? 
    dist_matrix <- distances(g)
    
    look_up <- dist_matrix[node_i,]; look_up[node_i] <- 0;
    weights <- 1 / (look_up + 1); weights[is.infinite(look_up)] <- 0.01;
    
    node_j <- sample(names(weights), size = 1, prob = weights)
    
    
  }
  
  for(node_i in (nodes_df$id %!in% c(active_nodes, remaining))) {
    
  }
   
 }
  
} # returns a list of edges_df across time t. 
