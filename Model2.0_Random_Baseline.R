










####  - Designate languages in play. ####
languages <- choose_local_languages(3)

#### - Designate the mortality hazard for the population ####
# choose the Siler model parameter values for the mortality regime your agents will experience.
CDW15 <- data.frame(a1 = 0.175, b1 = 1.4, a2 = 0.00368, a3 = 0.000075, b3 = 0.0917)

####  - Generate a starting set of agents, and their age structure ####
initial_ages <- generate_age_structure(n = 10000, mortality = CDW15, years = 300) # this line will take a few minutes to run.
agent_census <- make_basic_population(n_agents = 1000, age_distribution = initial_ages)

   ### Assign initial languages proficiencies for agents at Time 0
   # This function assumes an equal number of monolingual speakers for each local language and determines language proficiency by agent age.
    agent_census <- assign_starting_proficiency(agent_census)
  
   ### Initialize output table
    output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agent_census)))
    names(output) <- names(agent_census)
  
#### Set years of model run time. ####
  tmax = 1


start.time <- Sys.time()
    
    for(i in seq(tmax)){
        print(i) # Loop Counter will appear in the console to let you know how the model run is progressing. 
        
        #### DEMOGRAPHY ####
        # Calculate number of deaths there will be this year based on age structure of population.
        agent_census <- agent_census[which(is.na(agent_census$death_recorded)),]
        
        #  Pair up males/females for reproductive partnerships:
        agent_census <- select_marriage_partners(agent_census, calculate_dyad_score = calc_dyad_age_similarity)
        
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
        conversants <- sapply(seq(agent_census$agent_id), record_conversations, interactions)
        
        # -  Record which language each agent chooses to speak in each conversation:
        # Calculate a list of conversation languages heard by each agent
        languages_of_conversation <- lapply(conversants, select_language_of_conversation)
        
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
    
    
    
    
    
    