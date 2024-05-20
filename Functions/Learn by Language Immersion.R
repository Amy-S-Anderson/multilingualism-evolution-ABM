


#### Functions to Learn Languages from Conversational Exposure (immersion) ####

### Function to Record Conversations ###
# This function needs to:
# 1. Turn the 'interactions' matrix full of agent IDs into a list of conversation partners/conversations experienced by each agent.
# 2. To do this, it grabs every entry for each agent in the interactions matrix: all partners listed in the agent's ego row, and the agent ID for every time this agent was sampled in the ego row of another agent.

### Function to Ch

# I will want to take the output data frame from this function and use it as a running census of agent ages and deaths. 

########################################################################################




record_conversations <- function(agent_id, interactions = interactions){
  agent_id <- interactions[agent_id, 1]
  # subset interaction matrix: only the rows that contain agent i. 
  filtered_mat <- as.matrix(interactions[apply(interactions, 1, function(row) any(row == agent_id)), ]) # need to force this output into matrix form because it defaults to being a character vector if the ego agent isn't sampled in any of the alter rows, and then the next line throws an error when it's asked to subset a matrix (incorrect number of dimensions).
  
  # Extract all alter agent IDs from the rows where the agent's ID appears in the first column (agent i's ego row)
  interactions_from_ego_rows <- filtered_mat[filtered_mat[, 1] == agent_id, -1]
  
  # Extract all agent IDs from the first column, excluding the agent's own ID (agent i's appearance as an alter in other agents' ego rows)
  interactions_from_alter_rows <- filtered_mat[filtered_mat[ ,1]!= agent_id, 1] ### agent ID row is NOT ALWAYS ONE
  
  # Combine the two sets of interacted agents
  all_conversant_IDs <- c(interactions_from_ego_rows, interactions_from_alter_rows)
  
  return(all_conversant_IDs)
}


########################################################################################




#### Function to calculate each agent's level of exposure to each local language, relative to their exposure to the other local languages ####


# Again, this function is designed to run on a vector of conversations experienced by a single agent. To return values for all agents, use an apply function.
# on its own, it returns a vector of length(languages). 
# The language to which the agent is exposed most often will have an exposure value of 1. Other language exposures will be scaled relative to this. 



# conversation_languages_vector = a vector of language names, indicating the language spoken in each conversation that this agent experienced in this round of model time t.
# pop = a data frame of agent traits. Defaults to the agent_census data frame created by other model functions that should be called before this one. 
calculate_language_exposures <- function(conversation_languages_vector, pop = agent_census){
  
  languages <- agent_census %>%
    select(starts_with("Language")) %>%
    names()
  
  # count the number of times this agent experienced being spoken to in each language.
  exposure_count <- NA
  relative_exposures <- NA
  
  for(lang in seq(languages)){
    exposure_count[lang] <- length(conversation_languages_vector[which(conversation_languages_vector == languages[lang])])
  }
  
  relative_exposures <- exposure_count / max(exposure_count)
  return(relative_exposures)
}




########################################################################################



#### Function to LEARN LANGUAGES as a result of social interactions ####

# agent_language_exposures = the output of the calculate_language_exposures() function above, applied to the full agent_census data frame. Should be a data frame with ncols = length(languages) and nrows = nrow(agent_census)
learn_languages <- function(agent_language_exposures, pop = agent_census){
  
  # function for effect of age on language learning rate -- THIS WILL CHANGE once I have more information from linguists. 
  age_factor <- function(age){
    params <- data.frame(d = 18, a = 0.5, r0 = 9, tc = 0)
    params$r0 * (1 - (1 / (1 + exp(-params$a * (age - params$tc - params$d))))) + 0.5
  }
  
  
  age = seq(from = 0, to = 90)
  age_rate = sapply(age, FUN = age_factor)
  
  languages <- pop %>%
    select(starts_with("Language")) %>%
    names()
  
  for(i in seq_len(nrow(pop[1:10]))){
    for(lang in languages){
      pop[i, lang] <- pop[i, lang] + # current language proficiency
        (age_rate[pop[i, "age"] + 1] * agent_language_exposures[i, lang]) # newly gained language proficiency
      
      if (pop[i, lang] > 100) {
        pop[i, lang] <- 100 # set a proficiency ceiling at 100
      }
    }
  }
  
  return(pop)
}

