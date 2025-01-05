

#### FUNCTIONS FOR SURVIVING A KILLER LANGUAGE MODEL ####



#### DEMOGRAPHY ############################################################################################################################

#### Function to generate unique agent IDs ####

# This function needs to:
# 1. Generate IDs for all the agents present at Time 0
# 2. Generate a new, unique ID for each agent who is born during the model run time. 

# So, the function itself will generate a single ID, but will be built to check the ID value for the most recently assigned agent ID and begin counting from there. 
generate_agent_id <- function(start_value) { # the first start_value should be 0. But this means that inside the model, as new agents are born, the numeric portion of the previous agent's ID can be used as the start value to generate the next agent's ID. 
  #agent_number <- start_value + 1
  id <- start_value + 1
  #id <- paste("id", agent_number, sep = "_")
  return(id)
}





#### Generate Time0 Agents ####
start_cohort <- function(n, age, n_languages, speaker_freqs) {
  # Validate speaker_freqs
  if (length(speaker_freqs) != n_languages) {
    stop("The length of 'speaker_freqs' must match 'n_languages'.")
  }
  if (abs(sum(speaker_freqs) - 1) > 1e-6) {
    stop("The values in 'speaker_freqs' must sum to 1.")
  }
  
  # Generate the agent census dataframe
  agent_census <- data.frame(agent_id = sapply(seq(from = 0, length.out = n), FUN = generate_agent_id)) %>%
    # Uniform age structure
    mutate(age = age,
           # Initiate year record
           year = 0,
           household = seq(from = 1, length.out = n))
  
  # Create columns for language proficiency variables
  # Note: This can only handle up to 9 languages. More than this, and you will need to change the chartr() arguments.
  languages <- c(paste("Speaks", chartr("123456789", "ABCDEFGHI", seq(n_languages)), sep = " "),
                 paste("Understands", chartr("123456789", "ABCDEFGHI", seq(n_languages)), sep = " ")
  )
  agent_languages <- as.data.frame(matrix(0, nrow = nrow(agent_census), ncol = length(languages)))
  names(agent_languages) <- languages
  
  # Determine the number of agents per language based on speaker_freqs
  agent_counts <- round(speaker_freqs * n)
  total_assigned <- sum(agent_counts)
  
  # Handle rounding adjustment to ensure the total number of agents equals n
  if (total_assigned != n) {
    adjustment <- n - total_assigned
    agent_counts[which.max(speaker_freqs)] <- agent_counts[which.max(speaker_freqs)] + adjustment
  }
  
  # Assign languages to agents based on the calculated counts
  agent_language_assignment <- rep(seq(n_languages), times = agent_counts)
  
  for (i in 1:nrow(agent_census)) {
    # Determine the assigned language for this agent
    language_idx <- agent_language_assignment[i]
    language_name <- chartr("123456789", "ABCDEFGHI", language_idx)
    
    # Set their proficiency to 100 for the assigned language
    agent_languages[i, paste("Speaks", language_name, sep = " ")] <- 100
    agent_languages[i, paste("Understands", language_name, sep = " ")] <- 100
  }
  
  # Combine agent_census with agent_languages
  agent_census <- cbind(agent_census, agent_languages)
  
  return(agent_census)
}




#### Birth a new generation of agents ####

# This function, like the ones above, expects a data frame of agent traits. 
# parent_age = identifies the age of reproduction. Only agents who are this age will become new parents. 
# Each agent in the parent generation gives birth to one child. 
birth_new_cohort <- function(pop, parent_age){
  # Create a data frame with a single row of NA values.
  parents <- pop[which(pop$age == parent_age),]$agent_id
  if(length(parents) == 0){
    print("No agents are old enough to become new parents!")
  }
  newborns <- data.frame(matrix(0, nrow = length(parents), ncol = ncol(pop)))
  # Set the column names to match those of pop
  colnames(newborns) <- colnames(pop)
  
  newborns$household <- unique(pop[which(pop$agent_id == parents),]$household)
  newborns$agent_id <- sapply(seq(from = max(pop$agent_id), length.out = length(parents)), function(x) generate_agent_id(start_value = x))
  newborns$year <- pop[which(pop$age == parent_age),]$year
  newborns$age <- 0

  updated_agents_census <- rbind(pop, newborns)
  return(updated_agents_census)
}







#### AGENT INTERACTIONS ############################################################################################################################



#### Select conversation partners ####

# This function generates a matrix of agent identities for conversational interactions experienced by each agent in this step of model time. 
# Each row is comprised of conversational partners for a single agent. Because agents are sampled with replacement, the number of interactions is not constant across all agents, but the number of interactions does follow a uniform distribution, and agents do not have consistently higher or lower numbers of conversation partners across different steps of model time. 

# agents = a data frame of agents and their attributes
# n_interactions = an integer; the number of conversations to sample for each agent. Note that the number of interactions each agent has will be substantially higher than this, because agents are likely to be named in the partner choice of other agents. 
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







#### List each agent's conversation partners ####

# this function takes a matrix of agent IDs and returns a list of named character vectors. 
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







#### CHOOSE LANGUAGES ##############################################################################################################################

# 'speak L1' and 'speak prestige' are simpler commands that are defined directly in the model call. 



#### Select Language at Random from agent's known languages ####

# Function to select the conversational language spoken by an agent in a specific interaction -- PICK AT RANDOM.
# agents_in_interaction = a named list of a single vector; one list in the nested interactions_list list. 
# pop = a data frame of agents and their traits. Must include columns for language speaking skills and language understanding skills in each language that exists within the simulation. 

select_random_language = function(agents_to_speak, pop) {
  #get column names for degree of language understood
  understands <- names(pop)[which(startsWith(names(pop), "Understands"))]
  
  #extract degree of language understanding for each agent interacting with focal agent
  language_data <- pop[match(agents_to_speak, pop$agent_id), c("agent_id", understands)]
  
  #calculate sampling weights for each agent choosing the language (this will be 0 or 1)
  language_weights = matrix(0, nrow=nrow(language_data), ncol=length(understands))
  language_weights[language_data[,understands]>15] = 1
  
  has_speech = rowSums(language_weights) != 0 #vector of true/false, corresponding to agents_to_speak (and to rows in language_weights)
  
  #Sample spoken language, or use "none" if speechless
  spoken = rep("none", length(agents_to_speak)) #Replace "none" with whatever you want speechless agents to use
  if (any(has_speech)) { #only sample languages if at least one interacting agent has speech
    spoken[has_speech] = apply(language_weights[has_speech,,drop=FALSE], 1, function(probs) {paste0("Speaks ", 
                                                                                                    sample(str_sub(understands, -1, -1), 1, 
                                                                                                           prob=probs))})
  }
  return(spoken)
}




#### Select best-known Language #### 
# Function to choose the language with the highest speaking value as each agent's language to speak in a conversation. If an agent's highest speaking value is tied across multiple languages, sample the tied languages at random. 
# agents_to_speak =  a vector of agent IDs, probably from the interactions_list() of dyadic conversation partners
# pop = the main active data frame of agent attributes

select_best_language = function(agents_to_speak, pop) {
  #get column names for degree of language understood
  understands <- names(pop)[which(startsWith(names(pop), "Understands"))]
  #get column names for degree of language spoken
  speaks <- names(pop %>% select(starts_with("Speaks")))
  
  #extract degree of language understanding for each agent interacting with focal agent
  understanding_data <- pop %>% 
    filter(agent_id %in% agents_to_speak) %>%
    select(agent_id, all_of(understands)) %>%
    pivot_longer(cols = understands, names_to = "understands", values_to = "skill_level")
  
  speaking_data <- pop %>% 
    filter(agent_id %in% agents_to_speak) %>%
    select(agent_id, all_of(speaks)) %>%
    pivot_longer(cols = speaks, names_to = "speaks", values_to = "speaking_level")
  
  language_data <- merge(understanding_data, speaking_data, by = "agent_id") %>%
    filter(skill_level > 15)
  if(nrow(language_data > 0)){
    language_data <- language_data %>%
      group_by(agent_id) %>%
      filter(speaking_level == max(speaking_level, na.rm = T)) %>%
      mutate(chosen_language = sample(speaks, size = 1)) %>%
      select(agent_id, chosen_language)
  } 
    
  
  spoken = rep("none", length(agents_to_speak))
  has_speech = which(agents_to_speak %in% language_data$agent_id) #vector of true/false, corresponding to agents_to_speak
  if(nrow(language_data) > 0){   #only sample languages if at least one interacting agent has speech
    # Match agents_to_speak with their chosen language
    chosen_languages <- language_data$chosen_language[match(agents_to_speak[has_speech], language_data$agent_id)]
    
    # Assign chosen languages to the respective positions in the spoken vector
    spoken[has_speech] <- chosen_languages
    
  }
  
  return(spoken)
}





#### Select Heritage Language #### 
# Function to determine whether an agent has enough knowledge of their household's heritage language to choose to speak it in conversation with another agent. The knowledge threshold for this choice can be set at any value between 0 and 100. 
# If an agent *can* speak their heritage language well enough, then they will speak it. If they don't know their heritage language well enough, then they will default to speaking the language that they know best. 


# agents_to_speak = vector of agent IDs
# pop = data frame of agents (rows) and their traits (columns)
# heritage language = a data frame of two columns: household ID and name of that household's heritage language (the language assigned to the monolingual ancestor in the immaculate conception generation (generation 1))
# threshold_of_ability = a number between 0 and 100. The speaking value above which an agent *can* choose to speak their heritage language. This is likely to be quite high for parents choosing whether to speak this language to their children, but can be 0 for children just beginning to learn the language. 

select_heritage_language <- function(agents_to_speak, pop, heritage_language, threshold_of_ability){
  #get column names for degree of language understood
  understands <- names(pop)[which(startsWith(names(pop), "Understands"))]
  #get column names for degree of language spoken
  speaks <- names(pop %>% select(starts_with("Speaks")))
  
  #extract degree of language understanding for each agent interacting with focal agent
  understanding_data <- pop %>% 
    filter(agent_id %in% agents_to_speak) %>%
    select(agent_id, all_of(understands)) %>%
    pivot_longer(cols = understands, names_to = "understands", values_to = "skill_level") %>%
    filter(skill_level > (100/12) * 2) # retains only rows with data on a language that an agent understands well enough to speak it.
  
  # extract each agent's degree of speaking skill in each language.
  if(nrow(understanding_data) > 0){
  speaking_data <- pop %>% 
    filter(agent_id %in% understanding_data$agent_id) %>%
    select(agent_id, all_of(speaks), household) %>%
    pivot_longer(cols = speaks, names_to = "speaks", values_to = "speaking_level") %>%
    group_by(agent_id) %>%
    # identify which language is their best known language (and if languages are tied for best known, pick one of them)
    mutate(best_known = sample(speaks[which.max(speaking_level)], 1))
  
  
  #identify the heritage language in each household
  heritage = pop %>%
    filter(agent_id %in% understanding_data$agent_id) %>%
    merge(heritage_language, by = "household") %>%
    select(agent_id, L1)
  
  # identify hertiage language speakers in the agents_to_speak vector
  speaker_options <- speaking_data %>%
    merge(heritage_language, by = "household") %>%
    group_by(agent_id) %>%
    mutate(heritage_speaker = if_else(any(speaks == L1 & speaking_level > threshold_of_ability),"yes", "no")) %>%
    select(agent_id, best_known, heritage_speaker, L1) %>%
    distinct() %>%
    mutate(spoken = if_else(heritage_speaker == "yes", L1, best_known)) %>%
    select(agent_id, spoken) 
  }
  
  has_speech <- which(agents_to_speak %in% understanding_data$agent_id)  # Vector of true/false, corresponding to agents_to_speak
  # Match the chosen language to the agents_to_speak
  spoken <- rep("none", length(agents_to_speak))
  if (length(has_speech) > 0) {  # Only assign languages if at least one agent has a valid choice
    # Match agents_to_speak with their chosen language
    chosen_languages <- speaker_options$spoken[match(agents_to_speak[has_speech], speaker_options$agent_id)]
    
    # Assign chosen languages to the respective positions in the spoken vector
    spoken[has_speech] <- chosen_languages
  }
  
  return(spoken)
}






#### LEARN LANGUAGES ##############################################################################################################################
#### Calculate an agent's level of exposure to each language, relative to their exposure to the other local languages ####

# This function works both to count up the number of times each agent hears each language,
# as well as the number of times each agent speaks each language. 


# This function is designed to run on a vector of conversations experienced by a single agent. To return values for all agents, wrap this function in an apply() function.
# on its own, it returns a vector of length(languages). 
# If absolute_exposure = FALSE, then The language to which the agent is exposed most often will have an exposure value of 1. Other language exposures will be scaled relative to this. 
# If absolute_exposure = TRUE, then each agent's language exposure is scaled relative to a pre-determined value for the saturation exposure —- the number of conversations at which the gain in language proficiency reaches its ceiling. Additional conversations in this language after the saturation exposure has been reached will not improve an agent's proficiency gains in this language during this year of model time. 



# conversation_languages_vector = a vector of language names, indicating the language spoken in each conversation that this agent experienced in this round of model time t.
# pop = a data frame of agent traits.  
# absolute_exposure = a logical value indicating whether the effect of an agent's exposure to a given language will be scaled relative to an absolute count (the saturation exposure), or relative to the exposure count for their most commonly experienced language in that round of model time.
# Absolute exposure assumes that there are diminishing marginal returns for the effect of language exposure on gains in language proficiency. It also means that agents who have more exposure events (i.e., conversations with other agents) learn more than agents with fewer exposure events. 
# saturation_exposure = a number, only to be specified when absolute_exposure = TRUE. This is the exposure count that results in maximum proficiency gains in that language for this year of model time. 
# non_linear_scaling = a number. Defaults to 1 (linear scaling). If it isn't 1, then the relationship between language exposure and proficiency gain is non-linear. 
calculate_language_exposures <- function(conversation_languages_vector, pop, 
                                         absolute_exposure = FALSE, saturation_exposure = NULL, non_linear_scaling = 1){
  
  languages <- pop %>%
    select(starts_with("Speaks")) %>%
    names()
  
  # count the number of times this agent experienced being spoken to in each language.
  exposure_count <- NA
  relative_exposures <- NA
  
  for(lang in seq(languages)){
    if(length(conversation_languages_vector[which(conversation_languages_vector == languages[lang])]) > 0){
      exposure_count[lang] <- length(conversation_languages_vector[which(conversation_languages_vector == languages[lang])])
    } 
    if(length(conversation_languages_vector[which(conversation_languages_vector == languages[lang])]) == 0){
      exposure_count[lang] <- 0
    }
    
  }
  
  if(absolute_exposure == TRUE){ 
    relative_exposures <- (exposure_count / saturation_exposure)^non_linear_scaling
    if(relative_exposures < 1) {relative_exposures <- 1} # cap the possible proficiency gains for agents who exceed the saturation exposure for a language
  } else{
    relative_exposures <- if(max(exposure_count) == 0){
    exposure_count} else{(exposure_count / max(exposure_count))^non_linear_scaling }
  }
  
  return(relative_exposures)
}



 #test <- calculate_language_exposures(conversation_languages_vector, agents)





#### Update agents' Language Values ####


# This function updates individual agents' Understanding values for each language, as a function of their age and their exposure to each language in this step of model time.
# language exposures = a data frame produced by the 'calculate language exposures' function. It contains a row for each agent and a column for each language, populated with numeric values for language exposure. 
# pop = a data frame of agent traits
# language_skill = a character string, either "Understands" or "Speaks"


# Keeping it simple and going with a constant (age-independent) rate of learning. Assume that an agent who enters a monolingual environment will gain complete mastery of the language in 12 years, regardless of the age at which they enter that environment.

learn_languages_by_listening <- function(language_exposures, pop, learning_rate = 100/12){
  pop_languages <- names(pop %>% select(starts_with("Understands")))
  for(lang in pop_languages){
    pop[,lang] <- pop[,lang] + learning_rate * language_exposures[lang] # if they already have a value for this language, add this year's gain to the existing value
    pop[,lang] <- case_when(pop[,lang] > 100 ~ 100, # set a proficiency ceiling at 100
                            TRUE ~ as.numeric(pop[,lang]))
  }
  
  return(pop)
}




# Just like the function above, but with a two year lag time to account for the necessary scaffolding of understanding before speaking begins
learn_languages_by_speaking <- function(language_exposures, pop, learning_rate = 100/12){ 
 
  # speech_threshold = the speaking skill value of a monolingually exposed two-year-old. Once an agent reaches this value in their understanding skill of a language, they can choose to speak this language in interactions with other agents.  
  speech_threshold = learning_rate * 2
  
  # Columns for 'Speaks' and 'Understands'
  pop_speaks <- names(pop %>% select(starts_with("Speaks")))
  pop_understands <- names(pop %>% select(starts_with("Understands")))
  
  # Ensure 'Speaks' and 'Understands' columns are matched
  for(i in seq_along(pop_understands)){
    understand_col <- pop_understands[i]
    speak_col <- pop_speaks[i]
    
    # Apply the logic
    pop[[speak_col]] <- case_when(
      pop[[understand_col]] < speech_threshold ~ 0, # Set NA if < the understanding level of a two-year-old. 
      pop[[understand_col]] >= speech_threshold ~ pop[[speak_col]] + learning_rate * language_exposures[[speak_col]], # Update existing proficiency
      .default = pop[[speak_col]] # Default case
    )
    
    # Apply proficiency ceiling
    pop[[speak_col]] <- case_when(
      pop[[speak_col]] > 100 ~ 100, # Cap proficiency at 100
      .default = pop[[speak_col]]
    )
  }
  return(pop)
}












