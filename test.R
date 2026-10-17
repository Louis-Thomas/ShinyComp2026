library(igraph)
library(dplyr)

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

nodes_df <- node_preset_data
edges_df <- edge_preset_data

g <- graph_from_data_frame(
  d = edge_preset_data, 
  directed = FALSE,
  vertices = node_preset_data
)

dist_matrix <- distances(g)

participation_rate <- 0.7

run_ecosystem_sim <- function(nodes_df, edges_df, steps = 10) {
  
  edges_history <- vector("list", steps)
  current_edges <- edges_df
  
 for(t in 1:steps) {
  
  g <- graph_from_data_frame(d = current_edges, directed = FALSE, vertices = nodes_df)
  dist_matrix <- distances(g, weights = NA)
   
  active_nodes <- sample(nodes_df$id, size = ceiling(length(nodes_df$id)*participation_rate))
  remaining <- active_nodes
  
  while(length(remaining) > 1) {
    
    node_i <- sample(remaining, 1)
    
    look_up <- dist_matrix[node_i,]; look_up[node_i] <- 0;
    weights <- 1 / (look_up + 1); weights[node_i] <- 0; weights[is.infinite(look_up)] <- 0.01; weights[is.na(look_up)] <- 0;
    
    node_j <- sample(names(weights), size = 1, prob = weights)
    
    existing_edge <- which(
      (current_edges$from == node_i & current_edges$to == node_j) | 
      (current_edges$from == node_j & current_edges$to == node_i)
    )
    current_w <- if(length(existing_edge) > 0) existing_edge$width else 0
    
    delta <- nodin(node_i, node_j, current_w, dist_matrix[node_i, node_j], nodes_df)
    
    new_w <- pmin(max(current_w + delta, 0), 10)
    
    if(length(existing_edge) > 0) {
      if(new_w == 0){
        current_edges <- current_edges %>%
          filter(!((from == node_i & to == node_j) | (from == node_j & to == node_i)))
      } else {
        current_edges <- current_edges %>%
          mutate(width = if_else((from == node_i & to == node_j) | (from == node_j & to == node_i), new_w, width))
      }
    } else {
      if(new_w > 0) {
        current_edges <- bind_rows(current_edges, data.frame(from = node_i, to = node_j, width = new_w))
      }
    }
    
    remaining <- setdiff(remaining, c(node_i, node_j))
  }
  
  for(node_i in (nodes_df$id %!in% c(active_nodes, remaining))) {
    
  }
   
  edges_history[[t]] <- current_edges
  
 }
  
  return(edges_history)
  
} # returns a list of edges_df across time t. 

run_ecosystem_sim <- function(nodes_df, edges_df, steps = 10, participation_rate = 0.8) {
  
  edges_history <- vector("list", steps)
  current_edges <- edges_df
  
  for(t in 1:steps) {
    
    # 1. Build graph and compute distance matrix
    g <- graph_from_data_frame(d = current_edges, directed = FALSE, vertices = nodes_df)
    dist_matrix <- distances(g, weights = NA)
    
    # 2. Sample active nodes based on participation rate
    active_nodes <- sample(nodes_df$id, size = ceiling(length(nodes_df$id) * participation_rate))
    remaining <- active_nodes
    
    while(length(remaining) > 1) {
      
      node_i <- sample(remaining, 1)
      
      look_up <- dist_matrix[node_i, ]
      look_up[node_i] <- 0
      
      weights <- 1 / (look_up + 1)
      weights[node_i] <- 0
      weights[is.infinite(look_up)] <- 0.01
      weights[is.na(look_up)] <- 0
      
      node_j <- sample(names(weights), size = 1, prob = weights)
      
      # Find if edge exists (returns row indices)
      existing_edge <- which(
        (current_edges$from == node_i & current_edges$to == node_j) | 
          (current_edges$from == node_j & current_edges$to == node_i)
      )
      
      # FIX: Correctly look up the weight using row index
      current_w <- if(length(existing_edge) > 0) current_edges$width[existing_edge[1]] else 0
      
      # Calculate delta using your nodin function
      delta <- nodin(node_i, node_j, current_w, dist_matrix[node_i, node_j], nodes_df)
      
      new_w <- pmin(max(current_w + delta, 0), 10)
      
      # Update edges data frame
      if(length(existing_edge) > 0) {
        if(new_w == 0){
          current_edges <- current_edges %>%
            filter(!((from == node_i & to == node_j) | (from == node_j & to == node_i)))
        } else {
          current_edges <- current_edges %>%
            mutate(width = if_else((from == node_i & to == node_j) | (from == node_j & to == node_i), new_w, width))
        }
      } else {
        if(new_w > 0) {
          current_edges <- bind_rows(current_edges, data.frame(from = node_i, to = node_j, width = new_w))
        }
      }
      
      remaining <- setdiff(remaining, c(node_i, node_j))
    }
    
    # --- Inactive Node Decay (Relationship Fading) ---
    inactive_nodes <- setdiff(nodes_df$id, active_nodes)
    
    if (length(inactive_nodes) > 0) {
      inactive_edges_idx <- which(
        current_edges$from %in% inactive_nodes | current_edges$to %in% inactive_nodes
      )
      
      if (length(inactive_edges_idx) > 0) {
        random_decay <- runif(length(inactive_edges_idx), min = 0.01, max = 0.12)
        current_edges$width[inactive_edges_idx] <- current_edges$width[inactive_edges_idx] - random_decay
        current_edges <- current_edges %>% filter(width > 0)
      }
    }
    
    
    edges_history[[t]] <- current_edges
  }
  
  return(edges_history)
}

# delta variable function
nodin <- function(node_i, node_j, current_weight, distance_val, nodes_df) {
  
  # Look up sociability scores using string matching
  soc_i <- nodes_df$sociability[match(node_i, nodes_df$id)]
  soc_j <- nodes_df$sociability[match(node_j, nodes_df$id)]
  soc_avg <- mean(c(soc_i, soc_j))
  
  # Higher sociability increases the chance for positibe interactions
  p_positive <- min(max(soc_avg / 10, 0.1), 0.9) # bounds probability between 0.1 and 0.9
  event_valence <- sample(c(1, -1), size = 1, prob = c(p_positive, 1 - p_positive))
  
  # Buffering for closer together nodes, getting an inverse distrance metric.
  safe_dist <- if(is.infinite(distance_val) || distance_val <= 0) 0.1 else distance_val
  inv_distance <- 1 / safe_dist
  
  # Bufferring from current weight, where strongly connected nodes shouldn't have large negative interactions
  bond_strength_factor <- ifelse(is.na(current_weight)|current_weight == 0, 0.5, current_weight / 5)
  
  base_random_shock <- runif(1, min = 0.1, max = 2.0)
  
  magnitude <- base_random_shock * inv_distance * (soc_avg / 5) * (1 + bond_strength_factor)
  
  if(event_valence == -1 && safe_dist < 1.5) {
    magnitude <- magnitude * runif(1, 0.05, 0.3) # strong dampening for close-range friction
  }
  
  delta <- magnitude * event_valence
  
  return(round(delta, 2))
}
