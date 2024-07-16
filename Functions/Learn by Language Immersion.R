


#### Functions to Learn Languages from Conversational Exposure (immersion) ####

### Function to Record Conversations ###
# This function needs to:
# 1. Turn the 'interactions' matrix full of agent IDs into a list of conversation partners/conversations experienced by each agent.
# 2. To do this, it grabs every entry for each agent in the interactions matrix: all partners listed in the agent's ego row, and the agent ID for every time this agent was sampled in the ego row of another agent.

### Function to Ch

# I will want to take the output data frame from this function and use it as a running census of agent ages and deaths. 

########################################################################################


# agent_id, an index value identifying the position of the focal agent in the data set. 
# interactions = a matrix of agent IDs, with the first column as the focal agent and each row as a sample of ten conversations with other agents

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

# This function operates just like 'record_conversations()', but it includes the ego agent's ID as the first item in the vector of agent IDs. 
list_interactions <- function(agent_id, interactions = interactions){
  agent_id <- interactions[agent_id, 1]
  # subset interaction matrix: only the rows that contain agent i. 
  filtered_mat <- as.matrix(interactions[apply(interactions, 1, function(row) any(row == agent_id)), ]) # need to force this output into matrix form because it defaults to being a character vector if the ego agent isn't sampled in any of the alter rows, and then the next line throws an error when it's asked to subset a matrix (incorrect number of dimensions).
  
  # Extract all agent IDs from the row where the agent's ID appears in the first column (agent i's ego row)
  ego_row <- filtered_mat[filtered_mat[, 1] == agent_id, ]
  
  # Extract all agent IDs from the first column, excluding the agent's own ID (agent i's appearance as an alter in other agents' ego rows)
  alter_rows <- filtered_mat[filtered_mat[ ,1]!= agent_id, 1] ### agent ID row is NOT ALWAYS ONE
  
  # Combine the two sets of interacted agents
  all_conversant_IDs <- c(ego_row, alter_rows)
  
  return(all_conversant_IDs)
}


#### Function to calculate each agent's level of exposure to each local language, relative to their exposure to the other local languages ####


# Again, this function is designed to run on a vector of conversations experienced by a single agent. To return values for all agents, use an apply function.
# on its own, it returns a vector of length(languages). 
# If absolute_exposure = FALSE, then The language to which the agent is exposed most often will have an exposure value of 1. Other language exposures will be scaled relative to this. 
# If absolute_exposure = TRUE, then each agent's language exposure is scaled relative to a pre-determined value for the saturation exposure —- the number of conversations at which the gain in language proficiency reaches its ceiling. Additional conversations in this language after the saturation exposure has been reached will not improve an agent's proficiency gains in this language during this year of model time. 



# conversation_languages_vector = a vector of language names, indicating the language spoken in each conversation that this agent experienced in this round of model time t.
# pop = a data frame of agent traits. Defaults to the agent_census data frame created by other model functions that should be called before this one. 
# absolute_exposure = a logical value indicating whether the effect of an agent's exposure to a given language will be scaled relative to an absolute count (the saturation exposure), or relative to the exposure count for their most commonly experienced language in that round of model time.
  # Absolute exposure assumes that there are diminishing marginal returns for the effect of language exposure on gains in language proficiency. It also means that agents who have more exposure events (i.e., conversations with other agents) learn more than agents with fewer exposure events. 
# saturation_exposure = a number, only to be specified when absolute_exposure = TRUE. This is the exposure count that results in maximum proficiency gains in that language for this year of model time. 
# non_linear_scaling = a number. Defaults to 1 (linear scaling). If it isn't 1, then the relationship between language exposure and proficiency gain is non-linear. 
calculate_language_exposures <- function(conversation_languages_vector, pop = agent_census, 
                                         absolute_exposure = FALSE, saturation_exposure = NULL, non_linear_scaling = 1){
  
  languages <- agent_census %>%
    select(starts_with("Language")) %>%
    names()
  
  # count the number of times this agent experienced being spoken to in each language.
  exposure_count <- NA
  relative_exposures <- NA
  
  for(lang in seq(languages)){
    exposure_count[lang] <- length(conversation_languages_vector[which(conversation_languages_vector == languages[lang])])
  }
  
  if(absolute_exposure == TRUE){ 
    relative_exposures <- (exposure_count / saturation_exposure)^non_linear_scaling
    if(relative_exposures < 1) {relative_exposures <- 1} # cap the possible proficiency gains for agents who exceed the saturation exposure for a language
  } else{relative_exposures <- (exposure_count / max(exposure_count))^non_linear_scaling }
  
  return(relative_exposures)
}




########################################################################################



#### Function to LEARN LANGUAGES as a result of social interactions ####

# language_exposures = the output of the calculate_language_exposures() function above, applied to the full agent_census data frame. Should be a data frame with ncols = length(languages) and nrows = nrow(agent_census)
# pop = a data frame of agent characteristics, including agent ID, age, and current proficiency in each language
# pop_languages = a character vector of the names of the languages being simulated in the model space. 
learn_languages <- function(language_exposures = agent_language_exposures, pop = agent_census, pop_languages = languages){
  
  # effect of age on language learning rate -- THIS WILL CHANGE once I have more information from linguists. 
  ages = seq(from = 0, to = 120, by = 1)
  ages = ages[which(ages < max(agent_census$age))]
  params <- data.frame(d = 18, a = 0.5, r0 = 9, tc = 0)
  age_rate <-  sapply(ages, FUN = function(ages){
    params$r0 * (1 - (1 / (1 + exp(-params$a * (ages - params$tc - params$d))))) + 0.5
  })
  
  for(lang in pop_languages){
    pop[,lang] <- pop[,lang] +  # current language proficiency
      age_rate[pop$age + 1] * language_exposures[lang] # newly gained language proficiency
    
    pop[,lang] <- case_when(pop[,lang] > 100 ~ 100, # set a proficiency ceiling at 100
                            TRUE ~ as.numeric(pop[,lang]))
  }
  
  return(pop)
  
}
