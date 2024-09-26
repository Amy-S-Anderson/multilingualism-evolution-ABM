


#### Function to assign conversation partners ####


### Select Conversation Partners Function need to:
# 1. Loop across all agents, treating each agent in turn as the ego agent and sampling their conversation partners from the other agents
# 2. Account for user-specified interaction weighting based on
    # family relatedness (version 1: agent has higher prob. of interacting with their parents. Because interactions are two-way events, this means parents also have a higher prob. of interacting with their children.)
    # Age similarity (peer preference)

# These two features will change over an agent's lifetime. Future versions of this function will account for that, but right now they are stable characteristics of an agent's social landscape. 

# 3. Return a matrix of agent IDs in which the rows contain the ID of the ego agent (first column) and the IDs of their conversation partners. 


########################################################################################



#### Function: Select each agent's conversation partners at time t. 


# agent_census = a data frame holding agent traits at time t. It contains columns for unique agent ID, sex, age, mother and father's IDs, spouse ID, and language proficiency values for each language in the model space. 

# parent_weight = a numeric value. Default = 1, indicating that each agent's probability of interacting with a parent is equivalent to their probability of interacting with any other agent. 
# peer weight = like parent weight, but scaled for age similarity.

# When both parent_weight and peer_weight are set to 1 (the default values), agents interact with each other at random. 
select_conversation_partners <- function(agent_census, parent_weight = 1, peer_weight = 1){ ### Do I want to add an argument for peer preference (age homophily)? 
  
  # empty matrix of agent-agent interactions to be populated in this round of model time t. 
  interactions <- matrix(NA, nrow(agent_census), 11)
  
  # fill out the interaction matrix
  for (i in seq(agent_census$agent_id)){  
    ego <- agent_census$agent_id[i]
    ego_parents <- agent_census[which(agent_census$agent_id %in% c(agent_census$mother_id[i], agent_census$father_id[i])),]$agent_id
    alters <- agent_census[which(agent_census$agent_id != ego),]$agent_id
    
    parent_weighting <- if_else(alters %in% ego_parents, parent_weight, 1) #
    peer_weighting <- 1 # This doesn't do anything yet, but it's ready to be filled out with an if_else statement that returns a vector of interaction probability weights scaled by the ego agent's age similarity to each alter agent. 
    # each agent is exposed to 10 speech interactions, possibly repeats with the same person. 
    # each agent gets to be the ego for 10 interactions, but they will likely show up as an alter in another agent's ego row. As a result, agents experience a minimum of 10 conversations, but may experience many more. 
    conversants <- sample(alters, size = 10, replace = TRUE, prob = (parent_weighting * peer_weighting))
    interactions[i,] <- c(ego, conversants)
  }
  return(interactions)
}




########################################################################################


#### Functions for Model 3.0 ####

### This function generates a matrix of agent identities for conversational interactions. Each row conversational partners for a single agent. Because agents are sampled with replacement, some agents are likely to have more than 10 conversations (from being sampled in another agent's row of interactions).

# agents = a data frame of agents and their attributes
# n_interactions = an integer; the number of conversations to sample for each agent
# own_houshold_prob = a number between 0 and 1. This is a probability weighting that determines what proportion of an agent's interactions are with members of their same household vs. others outside their household. When set at 0.5, half of an agent's interactions are likely to be with agents in their own household. 
generate_interactions <- function(agents, n_interactions, own_household_prop) {
  n_agents <- nrow(agents)
  
  # Initialize a matrix to store interactions
  interaction_matrix <- matrix(NA, nrow = n_agents, ncol = n_interactions)
  
  # Calculate probabilities for each agent
  for (i in 1:n_agents) {
    ego <- agents$agent_id[i]
    alters <- agents[which(agents$agent_id != ego),]
    # Identify agents' household membership
    households <- alters$household
    # Determine which agents are in the same household as agent i
    same_household <- (households == agents$household[i]) 
    
    # Create probability vector: own household probability and others
    probability_ratio <- ifelse(same_household, 
                            own_household_prop / sum(same_household), 
                            (1 - own_household_prop) / sum(!same_household))
    #### * Note that, as written this means a own_household_prop value of 0.5 and a two-generation population size of 200
    # means that an agent is 65.33 times more likely to interact with a family member than a non-kin agent. 
    # It should work out that, on average, half of an agent's interactions are with a family member. 
    
    # Sample interactions for agent i based on calculated probabilities
    interaction_matrix[i, ] <- sample(alters$agent_id, n_interactions, replace = TRUE, prob = probability_ratio)
  }
  # 
  # # Reshape the interaction_matrix into a data frame with appropriate column names
  # interaction_df <- data.frame(
  #   agent_id = rep(agents$agent_id, each = n_interactions),
  #   interaction_with = as.vector(interaction_matrix)
  # )
  
  # return(interaction_df)
  return(interaction_matrix)
}




#### this function takes a matrix of agent IDs and returns a list of named character vectors. 
# The name of each resulting character vector corresponds to the ID of the agent who is the focal agent. The agent IDs within the list are those agents with whom the focal agentinteracted in this year of model time. 

# interaction_matrix = a matrix of agent IDs, with each row corresponding to the sampled conversants for each agent in the agent_census df
# agents = a data frame of agents and their traits. 

# Vectorized function to generate a list of all agents that each ego agent interacts with
get_interaction_lists <- function(interaction_matrix, agents) {
  # Number of agents
  n_agents <- nrow(agents)
  
  # Ego agent IDs
  ego_agent_ids <- agents$agent_id
  
  # Interaction matrix already contains interactions for each ego agent
  own_interactions <- split(interaction_matrix, row(interaction_matrix))
  
  # External interactions: identify where each ego agent appears in the matrix
  external_interactions <- lapply(ego_agent_ids, function(id) {
    which(interaction_matrix == id, arr.ind = TRUE)[, 1]
  })
  
  # Combine own interactions with external interactions for each agent
  combined_interactions <- mapply(function(own, external) {
    c(own, ego_agent_ids[external])
  }, own_interactions, external_interactions, SIMPLIFY = FALSE)
  
  # Assign names to the list for clarity
  names(combined_interactions) <- ego_agent_ids
  
  return(combined_interactions)
}


