
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
#### - Designate the mortality hazard for the population ####
# choose the Siler model parameter values for the mortality regime your agents will experience.
CDW15 <- data.frame(a1 = 0.175, b1 = 1.4, a2 = 0.00368, a3 = 0.000075, b3 = 0.0917)



#### DEFINE THE MODEL FUNCTION #### 
run_model <- function(N_LANGUAGES, # number of languages 
                      POP_SIZE, # number of agents
                      MORTALITY_HAZARD, # df of siler function parameter values
                      MIN_SPEAKING_PROFICIENCY, ### Assign min proficiency threshold for being able to speak a language
                      YEARS #### Set years of model run time. ####
){ 
  ####  - Designate languages in play. ####
  languages <- choose_local_languages(N_LANGUAGES)
  #### Generate Starting Population ####
  agent_census <- make_basic_population(n_agents = POP_SIZE, age_distribution = initial_ages) %>% # initial_ages is a vector in the global environment. See the Make file for details. 
    #### Assign initial languages proficiencies for agents at Time 0 ####
  # This function assumes an equal number of monolingual speakers for each local language and determines language proficiency by agent age.
  assign_starting_proficiency( languages = languages)
  
  ### Initialize output table
  output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agent_census)))
  names(output) <- names(agent_census)
  
  for(year in seq(YEARS)){
    # print(i) # Loop Counter will appear in the console to let you know how the model run is progressing. 
    
    
    #### DEMOGRAPHY ####
    
    # Wedding Season Match new spouses from the pool of eligible singles. 
    agent_census <- agent_census %>%
      filter(is.na(agent_census$death_recorded)) %>% # considering only agents who are alive in this year,
      #  Pair up males/females for reproductive partnerships:
      select_marriage_partners(calculate_dyad_score = calc_dyad_age_similarity) #### Marriage Compatibility = Age Similarity. ####
    
    # Calculate number of deaths there will be this year based on age structure of population.
    deaths <- reap(agent_census, mortality_regime = MORTALITY_HAZARD) # returns a list, with updated agent_census AND...
    turnover <- deaths$pop_turnover # ...the number of deaths predicted this year, based on the current pop. age structure. 
    # Update agent_census to include this year's deaths.
    agent_census <- deaths$agent_census
    # - People who lived through this round turn 1 year older.
    agent_census$age <- case_when(is.na(agent_census$death_recorded) ~ agent_census$age + 1,
                                  TRUE ~ agent_census$age)
    
    # Generate new births in existing partnerships. Assign traits to newborn agents. 
    new_parents <- sow_stationary(n_births = turnover, agent_census) # this function only applies to living women. 
    agent_census <- birth_new_agents(agent_census, new_parents)
    
    
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
    agent_census$year <- year
    
    # add the census for this year to the running total of data output
    output <- rbind(output, agent_census)
    # Repeat all of this living for the next value of time t.
    
  }
  return(output)
}




####  RUN THE MODEL #### 
RUNS = 10
start.time <- Sys.time()
iterations <- lapply(seq(RUNS), function(x) run_model(POP_SIZE = 1000, # number of agents 
                                                      N_LANGUAGES = 3, # number of languages 
                                                      MORTALITY_HAZARD = CDW15, # df of siler function parameter values
                                                      MIN_SPEAKING_PROFICIENCY = 20, 
                                                      YEARS = 200))
iterations <- mapply(cbind, iterations, run=seq(RUNS), SIMPLIFY=F) # add a column for the iteration number

#### OUTPUT ####  
final_output <- do.call(rbind, iterations)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken    



TEST <- run_model(POP_SIZE = 50, # number of agents 
                  N_LANGUAGES = 3, # number of languages 
                  MORTALITY_HAZARD = CDW15, # df of siler function parameter values
                  MIN_SPEAKING_PROFICIENCY = 20, 
                  YEARS = 10)

