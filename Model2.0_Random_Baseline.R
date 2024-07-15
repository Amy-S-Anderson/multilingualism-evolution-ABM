

# Radical Egalitarian Multilingualism

# This model scenario minimizes the number of structured choices in the model space. It keeps simplistic (and unrealistic) assumptions that:
 # - agents don't mind what languages their spouse is able to speak, but prefer partners closer to their own age
 # - agents choose which other agents to interact with by randomly sampling the other agents (with replacement)
 # - each round of model time, each agent picks 10 conversations (perhaps fewer than 10 conversants, given sampling with replacement). 
 # They may be named by other agents as well, so some agents will experience more than 10 conversations, but no one will experience < 10 conversations.
 # - agents have no preference which language they choose to speak in any given interaction.
 




#### SET UP MODEL SPACE #### 
#### - Designate the mortality hazard for the population ####
# choose the Siler model parameter values for the mortality regime your agents will experience.
CDW15 <- data.frame(a1 = 0.175, b1 = 1.4, a2 = 0.00368, a3 = 0.000075, b3 = 0.0917)

####  - Generate a starting set of agents, and their age structure ####
# initial_ages <- generate_age_structure(n = 1000, mortality = CDW15, years = 300) # this line will take a few minutes to run.
# initial_ages <- as.data.frame(initial_ages)
# write.csv(initial_ages, file = "starting_age_structure.csv")
initial_ages <- read.csv("starting_age_structure.csv")



#### DEFINE THE MODEL FUNCTION #### 
run_model <- function(pop_size, # number of agents 
                      n_languages, # number of languages 
                      mortality_hazard, # df of siler function parameter values
                      min_speaking_proficiency, ### Assign min proficiency threshold for being able to speak a language
                      years #### Set years of model run time. ####
){ 
  ####  - Designate languages in play. ####
  languages <- choose_local_languages(N_LANGUAGES)
  agent_census <- make_basic_population(n_agents = pop_size, age_distribution = initial_ages$initial_ages)
  ### Assign initial languages proficiencies for agents at Time 0
  # This function assumes an equal number of monolingual speakers for each local language and determines language proficiency by agent age.
  agent_census <- assign_starting_proficiency(agent_census, languages = languages)
  
  ### Initialize output table
  output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agent_census)))
  names(output) <- names(agent_census)
  
  for(year in seq(years)){
    # print(i) # Loop Counter will appear in the console to let you know how the model run is progressing. 
    
    #### DEMOGRAPHY ####
    # Calculate number of deaths there will be this year based on age structure of population.
    agent_census <- agent_census[which(is.na(agent_census$death_recorded)),] %>%
      
      #  Pair up males/females for reproductive partnerships:
      select_marriage_partners(calculate_dyad_score = calc_dyad_age_similarity)
    
    deaths <- reap(agent_census, mortality_regime = mortality_hazard)
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
    languages_of_conversation <- lapply(conversants, FUN = select_language_of_conversation_at_random)
    
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
iterations <- lapply(seq(runs), function(x) run_model(pop_size = 50, # number of agents 
                                                      n_languages = 3, # number of languages 
                                                      mortality_hazard = CDW15, # df of siler function parameter values
                                                      min_speaking_proficiency = 20, 
                                                      years = 10))

iterations <- mapply(cbind, res, run=seq(runs), SIMPLIFY=F) # add a column for the iteration number

#### OUTPUT ####  
final_output <- do.call(rbind, iterations)

    
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken    
    
    
    
    
    
    