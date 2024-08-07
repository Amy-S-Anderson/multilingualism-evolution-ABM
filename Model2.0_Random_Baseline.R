

# Radical Egalitarian Multilingualism

# This model scenario minimizes the number of structured choices in the model space. It keeps simplistic (and unrealistic) assumptions that:
 # - agents don't mind what languages their spouse is able to speak, but prefer partners closer to their own age
 # - agents choose which other agents to interact with by randomly sampling the other agents (with replacement)
 # - each round of model time, each agent picks 10 conversations (perhaps fewer than 10 conversants, given sampling with replacement). 
 # They may be named by other agents as well, so some agents will experience more than 10 conversations, but no one will experience < 10 conversations.
 # - agents have no preference which language they choose to speak in any given interaction.

#### OPTION 1: All Language Exposure is Relative ####
# This means that, when an agent's language exposure is calculated from their conversations, their exposure to each language
# is calculated relative to their exposure to their most-exposed language in that year.
# In theory then, an individual who is exposed equally to an infinite number of languages could become infinitely multilingual.
# Also, an individual who speaks to many more people does not have an advantage in learning more languages -- 
# -- except that a larger sample of conversations is more likely to be representative of the language diversity in the population of speakers.


#### OPTION 2: Language Exposure is Relative to an Absolute Saturation Point ####
# This means that, when an agent is exposed to more conversations, they can learn more languages.
# It also means that an agent whose exposure is split between too many languages without enough input, the agent won't master any one language
# So the outcome of this setup depends, in part, on the relationship between
# 1. the number of agent interactions per round, 
# 2. the number of languages in the space, and
# 3. the set value for the 'saturation point' of exposure to any one language. 


#### SET UP MODEL SPACE #### 
#### - Designate the mortality hazard for the population ####
# choose the Siler model parameter values for the mortality regime your agents will experience.
CDW15 <- data.frame(a1 = 0.175, b1 = 1.4, a2 = 0.00368, a3 = 0.000075, b3 = 0.0917)

POP_SIZE = 100

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
            assign_starting_proficiency(languages = languages)
  
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
    interactions <- select_conversation_partners(agent_census[which(is.na(agent_census$death_recorded)),])
    
    # - Now that the interaction matrix for this round is generated, Count up the number of interactions each agent has had.
    conversants <- sapply(seq(agent_census[which(is.na(agent_census$death_recorded)),]$agent_id), list_interactions, interactions)
    
    # -  Record which language each agent chooses to speak in each conversation:
    # Calculate a list of conversation languages heard by each agent
    languages_of_conversation <- lapply(conversants, FUN = select_language_of_conversation_at_random)
    
    # - Record each agent's relative frequency of exposure to each language in this year of conversations:
    relative_exposures <- lapply(languages_of_conversation, FUN = calculate_language_exposures)
    agent_language_exposures <- t(do.call(cbind, relative_exposures))

    ### *Learn Languages!*
    # - Increase each agent's language proficiency in the population languages as a function of:
    #   1. their exposure to the language
    #   2. their age at time of exposure t.
    agent_census[which(is.na(agent_census$death_recorded)),] <- learn_languages(language_exposures = agent_language_exposures, pop = agent_census[which(is.na(agent_census$death_recorded)),])
    
    # record the year
    agent_census$year <- year
    
    # add the census for this year to the running total of data output
    output <- rbind(output, agent_census)
    # Repeat all of this living for the next value of time t.
    
  }
  return(output)
}




####  RUN THE MODEL #### 
start.time <- Sys.time()
run1 <- run_model(POP_SIZE = 50, # number of agents 
          N_LANGUAGES = 3, # number of languages 
          MORTALITY_HAZARD = CDW15, # df of siler function parameter values
          MIN_SPEAKING_PROFICIENCY = 20, 
          YEARS = 100)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken  


RUNS = 10
start.time <- Sys.time()
iterations <- lapply(seq(RUNS), function(x) run_model(POP_SIZE = 50, # number of agents 
                                                      N_LANGUAGES = 3, # number of languages 
                                                      MORTALITY_HAZARD = CDW15, # df of siler function parameter values
                                                      MIN_SPEAKING_PROFICIENCY = 20, 
                                                      YEARS = 10))
iterations <- mapply(cbind, iterations, run=seq(RUNS), SIMPLIFY=F) # add a column for the iteration number

#### OUTPUT ####  
final_output <- do.call(rbind, iterations)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken    
    
    
    
write.csv(final_output, file = "random_baseline_output_3_langs.csv")
    
# calculate TFR
test <- final_output %>%
  filter(!is.na(mother_id)) %>%
  select(agent_id, mother_id, run) %>%
  unique() %>%
  group_by(run, mother_id) %>%
  summarise(n_children = n())  
  mean(test$n_children) # 2.86 children/woman. 