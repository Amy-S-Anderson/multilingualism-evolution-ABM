

#### FUNCTIONS FOR PARENTAL LANGUAGE CHOICE MODEL (4.0) ####



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
start_cohort <- function(n, age, n_languages) {
  # Generate the agent census dataframe
  agent_census <- data.frame(agent_id = sapply(seq(from = 0, length.out = n), FUN = generate_agent_id)) %>%
    # uniform age structure
    mutate(age = age,
           # initiate year record
           year = 0,
           household = NA)
  
  # Alternate assigning male and female state for each agent
  agent_census$female <- rep(c(0, 1), length.out = n)
  
  # Create columns for language proficiency variables
  languages <- c(paste("Speaks", chartr("123456789", "ABCDEFGHI", seq(n_languages)), sep = " "),
                 paste("Understands", chartr("123456789", "ABCDEFGHI", seq(n_languages)), sep = " ")
  )
  agent_languages <- as.data.frame(matrix(0, nrow = nrow(agent_census), ncol = length(languages)))
  names(agent_languages) <- languages
  
  # Divide agents equally among languages
  for (i in 1:nrow(agent_census)) {
    # Determine which language this agent will speak and understand
    language_idx <- (i - 1) %% n_languages + 1
    language_name <- chartr("123456789", "ABCDEFGHI", language_idx)
    
    # Set their proficiency to 100 for the assigned language
    agent_languages[i, paste("Speaks", language_name, sep = " ")] <- 100
    agent_languages[i, paste("Understands", language_name, sep = " ")] <- 100
  }
  
  # Combine agent_census with agent_languages
  agent_census <- cbind(agent_census, agent_languages)
  
  return(agent_census)
}




#### Marry agents to other agents in their generation ####

# This function expects a data frame of agent traits, as generated by start_cohort().
# It matches male-female dyads from the generation of 25-year-olds. Marriage partners are chosen without reference to any individual traits other than age matching and sex difference. 
marry_random <- function(agent_census){
  # Identify the eligible singles
  women <- subset(agent_census, age == 25 & female == 1)
  men <- subset(agent_census, age == 25 & female == 0)
  
  # Randomly shuffle men to create random pairings
  shuffled_men <- men[sample(nrow(men)), ]
  
  # generate household ID numbers for new couples
  max_household_id <- ifelse(all(is.na(agent_census$household)), 0, max(agent_census$household, na.rm = TRUE))
  household_ids <- seq(from = max_household_id + 1, length.out = nrow(women))
  
  # Assign household IDs to the paired couples
  women$household <- household_ids
  shuffled_men$household <- household_ids
  
  # Update the agent_census data frame with the new household IDs
  agent_census$household[match(women$agent_id, agent_census$agent_id)] <- women$household
  agent_census$household[match(shuffled_men$agent_id, agent_census$agent_id)] <- shuffled_men$household
  
  return(agent_census)
  
}







#### Birth a new generation of agents ####

# This function, like the ones above, expects a data frame of agent traits. This df must include married couples who share a household ID.
# For each married couple, this function generates fraternal twins — one boy, one girl — who share their parents' household ID. 
birth_new_cohort <- function(agent_census){
  # Create a data frame with a single row of NA values
  newborns <- data.frame(matrix(NA, nrow = length(agent_census[which(agent_census$age == 25),]$age), ncol = ncol(agent_census)))
  # Set the column names to match those of agent_census
  colnames(newborns) <- colnames(agent_census)
  
  household_ids <- unique(agent_census[which(agent_census$age == 25),]$household)
  newborns$agent_id <- sapply(seq(from = max(agent_census$agent_id), #as.numeric(substr(agent_census$agent_id, 4, nchar(agent_census$agent_id)))),
                                  length.out = length(agent_census[which(agent_census$age == 25),]$age) ), 
                              generate_agent_id)
  newborns$year <- agent_census$year
  newborns$age <- 0
  newborns$female <- c(rep(0, length(agent_census[which(agent_census$age == 25),]$age)/2), rep(1, length(agent_census[which(agent_census$age == 25),]$age)/2))
  newborns$household <- c(rep(household_ids, 2))
  newborns$generation <- max(agent_census$generation) + 1
  
  updated_agents_census <- rbind(agent_census, newborns)
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



#### Select Language at Random from agent's known languages ####

# Function to select the conversational language spoken by an agent in a specific interaction -- PICK AT RANDOM.
# agents_in_interaction = a named list of a single vector; one list in the nested interactions_list list. 
# pop = a data frame of agents and their traits. Must include columns for language speaking skills and language understanding skills in each language that exists within the simulation. 

select_language_at_random_to_speak <- function(agents_in_interaction, pop) {
  # Extract the relevant columns once
  speaks <- names(pop)[which(startsWith(names(pop), "Speaks"))]
  language_data <- pop[pop$agent_id %in% agents_in_interaction, c("agent_id", speaks)] 
  
  language_data <- language_data %>%
    pivot_longer(cols = starts_with("Speaks"), names_to = "can_speak", values_to = "Transmission") %>%
    group_by(agent_id) %>%
    mutate(Transmission_sum = sum(Transmission, na.rm = TRUE))
  
  # Identify agents who cannot speak any language
  if (nrow(language_data[which(language_data$Transmission_sum == 0), ]) > 0) {
    speechless <- data.frame(agent_id = language_data[which(language_data$Transmission_sum == 0),]$agent_id, 
                             spoken = NA) %>% distinct()
  } else {
    # If no speechless agents, initialize an empty data frame
    speechless <- data.frame(agent_id = character(0), spoken = character(0))
  }
  
  # Identify agents who can speak at least one language
  if (nrow(language_data[which(language_data$Transmission_sum > 0), ]) > 0) {
    speakers <- language_data %>%
      filter(Transmission > 0) %>% 
      group_by(agent_id) %>%
      summarise(spoken = sample(can_speak, size = 1)) %>%
      ungroup()
  } else {
    # If no speakers, initialize an empty data frame
    speakers <- data.frame(agent_id = character(0), spoken = character(0))
  }
  
  # Combine the results
  language_chosen <- rbind(speakers, speechless) 
  
  # Ensure the order matches the original agents_in_interaction order
  spoken <- language_chosen$spoken[match(agents_in_interaction, language_chosen$agent_id)] 
  
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
    exposure_count[lang] <- length(conversation_languages_vector[which(conversation_languages_vector == languages[lang])])
  }
  
  if(absolute_exposure == TRUE){ 
    relative_exposures <- (exposure_count / saturation_exposure)^non_linear_scaling
    if(relative_exposures < 1) {relative_exposures <- 1} # cap the possible proficiency gains for agents who exceed the saturation exposure for a language
  } else{relative_exposures <- (exposure_count / max(exposure_count))^non_linear_scaling }
  
  return(relative_exposures)
}







#### Update agents' Language Values ####
# This function updates individual agents' Understanding values for each language, as a function of their age and their exposure to each language in this step of model time.
# language exposures = a data frame produced by the 'calculate language exposures' function. It contains a row for each agent and a column for each language, populated with numeric values for language exposure. 
# pop = a data frame of agent traits
# language_skill = a character string, either "Understands" or "Speaks"
learn_languages_by_listening <- function(language_exposures, pop){
  
  # effect of age on language learning rate -- THIS WILL CHANGE once I have more information from linguists. 
  ages = seq(from = 0, to = 51, by = 1)
  params <- data.frame(d = 18, a = 0.2, r0 = 9)
  age_rate <-  params$r0 * (1 - (1 / (1 + exp(-params$a * (ages - params$d))))) + 10 # baseline 10% annually under conditions of maximal immersion, regardless of age
  #### Also, need to make sure Understanding reaches toddler level before Speaking can be populated. 
  
  pop_languages <- names(pop %>% select(starts_with("Understands")))
  for(lang in pop_languages){
      pop[,lang] <- case_when(is.na(pop[,lang]) & language_exposures[lang] > 0 ~ age_rate[pop$age + 1] * language_exposures[lang], # if they encountered it for the first time this year, then replace NA with the language gain from this year.
                            TRUE ~ pop[,lang] + age_rate[pop$age + 1] * language_exposures[lang]) # if they already have a value for this language, add this year's gain to the existing value
    pop[,lang] <- case_when(pop[,lang] > 100 ~ 100, # set a proficiency ceiling at 100
                            TRUE ~ as.numeric(pop[,lang]))
  }
  
  return(pop)
}







# Just like the function above, but with a two year lag time to account for the necessary scaffolding of understanding before speaking begins
learn_languages_by_speaking <- function(language_exposures, pop){
  
  # Effect of age on language learning rate
  ages <- seq(from = 0, to = 51, by = 1)
  params <- data.frame(d = 18, a = 0.2, r0 = 9)
  age_rate <- params$r0 * (1 - (1 / (1 + exp(-params$a * (ages - params$d))))) + 0.5
  
  # Columns for 'Speaks' and 'Understands'
  pop_speaks <- names(pop %>% select(starts_with("Speaks")))
  pop_understands <- names(pop %>% select(starts_with("Understands")))
  
  # Ensure 'Speaks' and 'Understands' columns are matched
  for(i in seq_along(pop_understands)){
    understand_col <- pop_understands[i]
    speak_col <- pop_speaks[i]
    
    # Apply the logic
    pop[[speak_col]] <- case_when(
      pop[[understand_col]] < 15 ~ NA_real_, # Set NA if Understands < 20, roughly the understanding level of a two-year-old. 
      pop[[understand_col]] >= 15 & is.na(pop[[speak_col]]) & language_exposures[[speak_col]] > 0 ~ age_rate[pop$age + 1] * language_exposures[[speak_col]], # New language learning
      pop[[understand_col]] >= 15 ~ pop[[speak_col]] + age_rate[pop$age + 1] * language_exposures[[speak_col]], # Update existing proficiency
      TRUE ~ pop[[speak_col]] # Default case
    )
    
    # Apply proficiency ceiling
    pop[[speak_col]] <- case_when(
      pop[[speak_col]] > 100 ~ 100, # Cap proficiency at 100
      TRUE ~ pop[[speak_col]]
    )
  }
  return(pop)
}









