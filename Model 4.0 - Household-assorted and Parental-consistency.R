


# Household-weighted assortment for agent-agent interactions + Consistency in parental language choice


### In this world, everyone has an 50% probability of interacting with someone outside their four-person nuclear household and a 50% probability of interacting with someone inside their four-person nuclear household.  

### When an individual interacts with someone, they choose their language of conversation by picking at random from among the languages that this individual knows how to speak. Their conversation partner does the same. As a result, many conversations may be conducted bilingually. 

### Each parent chooses one of their known languages at random to speak to each of their children, and then remains consistent in this choice across all interactions with each child. 




# Marry at Random with regard to partner's Languages
# Every year, everyone picks ten conversation partners (with replacement) from the population. Because other agents may also pick them, each agent has an average of 22 conversations each year. 
# In every conversation, each partner speaks by picking at random from among their known languages. Conversation partners do not need to coordinate on a single language within their conversation. 

#### GENERATE POPULATION DEMOGRAPHY #### 
# Generate first parent cohort, all age 25, all monolingual in one of five languages (A-E)
agents <- start_cohort(n = 100, age = 25, n_languages = 5)

# agents <- cbind(agents, first_language)
agents$generation <- 0

# Marry off the first cohort to each other, at random. 
agents <- marry_random(agents) %>%
  # birth new cohort (children of parent cohort)
  birth_new_cohort()

# There are now 200 agents: 100 25-year-old parents, and 100 newborn fraternal twins. One boy and one girl in each family. 
# Initialize output table

output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agents)))
names(output) <- names(agents)

# set number of generations to run
generations = 5
for(g in seq(generations)){
  print(paste("generation", g, sep = " ")) # Loop Counter in console will tell you which generation is growing up right now. 
  
  # initialize list of languages spoken by parents to their children
  children <- agents[which(agents$generation == max(agents$generation)),] %>%
    select(household, agent_id) %>%
    rename("child" = "agent_id")
  parents <- agents[which(agents$generation == min(agents$generation)),] %>%
    select(household, agent_id) %>%
    rename("parent" = "agent_id")
  parent_language <- merge(parents, children, by = "household")
  # For each of their two children, ach parent selects one of his/her known languages at random (with replacement).
  parent_language$parent_language <- select_language_at_random_to_speak(parent_language$parent) 
  # Parents will always speak the same language with the same child, but they may speak to each of their children in a different language. 
  #### Heidi, Is this the right assumption? Or should each parent speak only a single language to ALL their children? ####
  
  
  # Set years of model run time.
  tmax = 25
  
  for(i in seq(tmax)){
    print(paste("year", i, sep = " ")) # Loop Counter will appear in the console to let you know how the model run is progressing. 
    
    agents$year <- max(agents$year) + 1 
    
    #### MAKE AGENTS SPEAK TO EACH OTHER ####
    #### Generate conversation dyadic interactions for each agent ####
    interaction_matrix <- generate_interactions(agents, n_interactions = 10, own_household_prop = 0.5) 
    # The value for own_household_prop should result in roughly half of each agent's interactions being with members of their same household. 1/3 of their interactions should be with their parents; 1/6 of their interactions are with their same-aged sibling. 
    
    # Now, tally up the full list of conversants/conversations from this matrix. 
    ### Get the interaction list
    interaction_list <- get_interaction_lists(interaction_matrix, agents)
    # count up the number of 'conversations' had by each agent in this year. 
    # interactions_per_agent <- sapply(interaction_list, length)
    
    
    
    #### Each agent chooses the language they speak in each interaction ####
    # This empty list will be populated with the language spoken by each agent in the corresponding position to their agent ID in the interaction_list
    language_of_conversation <- list()
    
    for(i in 1:length(interaction_list)){  
      #  print(i) # for identifyng where in the for loop code errors are happening. 
      focal_agent <- as.numeric(names(interaction_list[i])) # identify focal agent
      # Identify their family members from the agent trait data frame
      family <- agents[which(agents$household == agents[which(agents$agent_id == focal_agent),]$household & agents$agent_id != focal_agent),]
      parents <- family[which(family$age > agents[which(agents$agent_id == focal_agent),]$age),]  # identify the focal agent's parents
      
      
      # Step 1: Get agent IDs from the named vector single_agent_interactions
      focal_agent_index <- which(agents$agent_id == focal_agent)
      agents_in_interaction <- interaction_list[[i]]
      
      # Step 2: Find the indices of agent IDs in interaction_list[1] that are in the 'parents' data frame
      parent_indices <- which(agents_in_interaction %in% parents$agent_id)
      # Step 3: Find the indices of agent IDs in the vector of interactions that are NOT parents (i.e., everyone else)
      other_indices <- which(!(agents_in_interaction %in% parents$agent_id))
      
      
      # Apply Language Choice Rules
      # Step 4: Apply Others Language Choice Rule to non-parent agents: Here, pick at random
      #### Non-parents select one of their known languages at random ####
      agents_in_interaction[other_indices] <- select_language_at_random_to_speak(agents_in_interaction[other_indices])
      
      # Step 5: Apply Parental Language Choice Rule to Parent Agents: Here, each parent has chosen one of their known languages at random,
      # and they will be consistent about speaking this chosen language to this particular child of theirs. They may speak a different language to their other child. 
      #### Parents speaking to their children with a randomly chosen (but consistently used) language ####
      if(length(parent_indices) > 0){
        for(j in unique(agents_in_interaction[parent_indices])){
          agents_in_interaction[which(agents_in_interaction == j)] <- parent_language[which(parent_language$child == focal_agent  & parent_language$parent == j),]$parent_language
        }
      } 
      
      language_of_conversation[[i]] <- agents_in_interaction
    }
    
    
    
    
    #### AGENTS LEARN LANGUAGES FROM THEIR INTERACTIONS #### 
    
    #### Calculate each agent's annual *listening* experience with each language ####  
    ### Tally up agent languages heard, in order to calculate language exposure for focal agents. This will improve their language understanding. 
    annual_listening_experience <- list()
    for(i in 1:length(language_of_conversation)){
      annual_listening_experience[[i]] <- calculate_language_exposures(conversation_languages_vector = language_of_conversation[[i]])
    }
    annual_listening_experience <- as.data.frame(do.call(rbind, annual_listening_experience))
    names(annual_listening_experience) <- names(agents %>% select(starts_with("Understands")))
    
    
    #### Calculate each agent's annual *speaking* experience with each language ####  
    ### Reorganize speaker/spoken lists so each list contains the languages spoken by the focal agent in each of their dyadic exchanges.
    #initialize empty list
    annual_speaking_experience <- list()
    # turn speaker IDs into one long vector for easy indexing
    all_speakers <- interaction_list %>% unname() %>% unlist()
    # do the same with the matching list of languages spoken in each conversation by each speaker
    all_spoken <- language_of_conversation %>% unname() %>% unlist()
    # rename a vector for agent IDs, for ease
    agent_ids <- names(interaction_list)
    
    # I'm sure there's a way to vectorize this for efficiency, but for now...
    languages_spoken <- list()
    for(i in 1:length(agent_ids)){
      speaker_index <- which(all_speakers == agent_ids[i]) # identify the positions at which this agent is named as a speaker
      languages_spoken[[i]] <- all_spoken[speaker_index] # identify the languages they chose to speak on each occasion
      
    }
    names(annual_listening_experience) <- names(agents %>% select(starts_with("Understands")))
    
    
    # Now, tally up each agent's experience speaking each language this year. 
    #initialize empty list
    annual_speaking_experience <- list()
    for(i in 1:length(languages_spoken)){
      annual_speaking_experience[[i]] <- calculate_language_exposures(conversation_languages_vector = language_of_conversation[[i]])
    }  
    annual_speaking_experience <- as.data.frame(do.call(rbind, annual_speaking_experience))
    names(annual_speaking_experience) <- names(agents %>% select(starts_with("Speaks")))
    
    
    # calculate agent improvements in language speaking
    ## Note: If you need to adjust the rate of language learning, go change the parameters under the hood for each of these functions. ##
    agents <- learn_languages_by_listening(annual_listening_experience) 
    agents <- learn_languages_by_speaking(annual_speaking_experience)
    
    # update L1 variable. 
    if(length(agents[which(is.na(agents$first_language)),]$first_language > 0)){ # If anyone is still missing an entry for their first language (L1)
      L1_update <- agents[which(is.na(agents$first_language)),] %>% select(agent_id, starts_with("Speaks")) %>%
        pivot_longer(cols = starts_with("Speaks"), values_to = "fluency", names_to = "first_language")# then see if they have one yet
      first_language <- rbind(first_language, L1_update)
    } ### Need to make sure 'speak L1' function can handle Bilinguals....
    
    
    # add the census for this year to the running total of data output
    output <- rbind(output, agents)
    
    # everyone gets one year older
    agents$age <- agents$age + 1
    # Repeat all of this living for the next value of time t.
    
    
  }
  
  
  ### In year 26:
  # - The parent generation dies
  # - The offspring generation marries
  # - Each married pair in the offspring generation gives birth to twins; This is now the new parent generation.
  
  # Kill the parent generation
  agents <- agents %>%
    filter(generation == max(generation))
  # Marry off the fully grown cohort to each other, at random. 
  agents <- marry_random(agents) %>%
    # birth new cohort (children of new parent cohort)
    birth_new_cohort()  
}




