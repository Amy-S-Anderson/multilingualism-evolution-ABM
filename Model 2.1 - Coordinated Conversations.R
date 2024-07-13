
##### MODEL 2.1 -- COORDINATED CONVERSATIONS. 

# This model scenario adjusts the randomized space of model 2.0 by adding one key constraint: 
### Agents in conversation with another agent must coordinate their choice of language for that conversation ###
### The dyad will choose whichever language maximizes the minimum proficiency of either agent.
### If agents do not share a language, but both have proficiency in non-overlapping languages, then each agent speaks to the other in their own language of maximum proficiency
### Babies don't speak.

# Additionally, as before:
# - agents don't mind what languages their spouse is able to speak, but prefer partners closer to their own age
# - agents choose which other agents to interact with by randomly sampling the other agents (with replacement)
# - each round of model time, each agent picks 10 conversations (perhaps fewer than 10 conversants, given sampling with replacement). 
# They may be named by other agents as well, so some agents will experience more than 10 conversations, but no one will experience < 10 conversations.




#### SET UP MODEL SPACE #### 

####  - Designate languages in play. ####
languages <- choose_local_languages(3)

#### - Designate the mortality hazard for the population ####
# choose the Siler model parameter values for the mortality regime your agents will experience.
CDW15 <- data.frame(a1 = 0.175, b1 = 1.4, a2 = 0.00368, a3 = 0.000075, b3 = 0.0917)

#### - Designate the marriage compatibility rules for the population ####
make_matches <- calc_dyad_age_and_place

####  - Generate a starting set of agents, and their age structure ####
# initial_ages <- generate_age_structure(n = 1000, mortality = CDW15, years = 300) # this line will take a few minutes to run.
# initial_ages <- as.data.frame(initial_ages)
# write.csv(initial_ages, file = "starting_age_structure.csv")
initial_ages <- read.csv("starting_age_structure.csv")
agent_census <- make_basic_population(n_agents = 1000, age_distribution = initial_ages$initial_ages)

### Assign initial languages proficiencies for agents at Time 0
# This function assumes an equal number of monolingual speakers for each local language and determines language proficiency by agent age.
agent_census <- assign_starting_proficiency(agent_census, languages = languages)

### Assign min proficiency threshold for being able to speak a language
min_speaking_proficiency <- 20

### Initialize output table
output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agent_census)))
names(output) <- names(agent_census)

#### Set years of model run time. ####
tmax = 100


start.time <- Sys.time()

for(i in seq(tmax)){
  print(i) # Loop Counter will appear in the console to let you know how the model run is progressing. 
  
  #### DEMOGRAPHY ####
  # Calculate number of deaths there will be this year based on age structure of population.
  agent_census <- agent_census[which(is.na(agent_census$death_recorded)),]
  
  #  Pair up males/females for reproductive partnerships:
  agent_census$place_id <- c(rep(1,500), rep(2,500))
  test <- select_marriage_partners(agent_census, calculate_dyad_score = calc_dyad_age_and_place)
  
  deaths <- reap(agent_census, mortality_regime = CDW15)
  turnover <- deaths$pop_turnover
  # Record this year's deaths
  agent_census <- deaths$agent_census
  
  # - People who lived through this round turn 1 year older
  agent_census$age <- case_when(is.na(agent_census$death_recorded) ~ agent_census$age + 1,
                                TRUE ~ agent_census$age)
  
  # - Generate new births in existing partnerships. Assign traits to newborn agents. 
  new_mothers <- sow_stationary(n_births = turnover, agent_census) # this function only applies to living women. 
  agent_census <- birth_new_agents(agent_census, new_mothers)
  
  
  #### SOCIAL LEARNING ####
  # - Pick conversation partners for the year.
  # This function assigns partners by sampling at random, with replacement. Each agent will experience a minimum of 10 conversations.
  interactions <- select_conversation_partners(agent_census)
  
  # - Now that the interaction matrix for this round is generated, Count up the number of interactions each agent has had.
  conversants <- sapply(seq(agent_census$agent_id), list_interactions, interactions)
  
  # -  Record which language each agent chooses to speak in each conversation:
  # Calculate a list of conversation languages heard by each agent
  languages_of_conversation <- lapply(conversants, FUN = select_language_of_conversation_max_proficiency)

    # - Record each agent's relative frequency of exposure to each language in this year of conversations:
  relative_exposures <- lapply(languages_of_conversation, FUN = calculate_language_exposures)
  
  # 
  agent_language_exposures <- as.data.frame(matrix(unlist(relative_exposures), ncol = 3))
  colnames(agent_language_exposures) <- languages
  
  ### *Learn Languages!*
  # - Increase each agent's language proficiency in the population languages as a function of:
  #   1. their exposure to the language
  #   2. their age at time of exposure t.
  agent_census <- learn_languages(language_exposures = agent_language_exposures, pop = agent_census)
  
  # record the year
  agent_census$year <- i
  
  # add the census for this year to the running total of data output
  output <- rbind(output, agent_census)
  # Repeat all of this living for the next value of time t.
  
}


end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken    


