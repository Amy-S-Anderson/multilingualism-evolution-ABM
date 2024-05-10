

######## Pair agents for conversation ###########

pair_agents_randomly <- function(N) { # N = number of agents
  # Shuffle the agents
  indices <- sample(1:N)
  
  # Pair the shuffled agents
  pairs <- matrix(indices, ncol = 2, byrow = TRUE)
  
  return(pairs)
}

