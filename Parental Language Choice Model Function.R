



# Random Baseline 

### In this world, everyone has an equal probability of interacting with everyone else. 
### When an individual interacts with someone, they choose their language of conversation by picking at random from among the languages that this individual knows how to speak. Their conversation partner does the same. As a result, many conversations may be conducted bilingually. 

set.seed(0)


# Marry at Random with regard to partner's Languages
# Every year, everyone picks ten conversation partners (with replacement) from the population. Because other agents may also pick them, each agent has an average of 22 conversations each year. 
# In every conversation, each partner speaks by picking at random from among their known languages. Conversation partners do not need to coordinate on a single language within their conversation. 

# parent_language_choice = a character string denoting the language choice logic used by a parent speaking to their child, either "random consistent" or "L1".
# others_language_choice = a character string denoting the language choice logic used by an agent speaking to any agen who is not their child, either "random" or "prestige_A". 
run_ABM <- function(generation_size, 
                               generations_n, 
                               household_interaction_prob,
                               parent_language_choice,
                               others_language_choice){
  
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

# run for generations_n number of generations
for(g in seq(generations_n)){
  print(paste("generation", g, sep = " ")) # Loop Counter in console will tell you which generation is growing up right now. 
  
  
  children <- agents[which(agents$generation == max(agents$generation)),] %>%
    select(household, agent_id) %>%
    rename("child" = "agent_id")
  parents <- agents[which(agents$generation == min(agents$generation)),] %>%
    select(household, agent_id) %>%
    rename("parent" = "agent_id")
  
  if(parent_language_choice == "random consistent"){
    # initialize list of languages spoken by parents to their children
    parent_language <- merge(parents, children, by = "household")
    # For each of their two children, each parent selects one of his/her known languages at random (with replacement).
    parent_language$parent_language <- select_language_at_random_to_speak(parent_language$parent, pop = agents) 
    # Parents will always speak the same language with the same child, but they may speak to each of their children in a different language. 
    
  }
  
  if(parent_language_choice == "L1"){
    # Record parents' first language
    parents_L1 <- agents[which(agents$agent_id %in% parents$parent),] %>% select(agent_id, household, age, starts_with("Speaks")) %>%
      pivot_longer(cols = starts_with("Speaks"), values_to = "fluency", names_to = "first_language") %>%
      filter(!is.na(fluency)) %>%
      group_by(agent_id) %>%
      filter(age == min(age)) %>%
      rename("parent" = "agent_id") %>%
      mutate(L1_count = n(),
             number_of_L1s = case_when(L1_count == 1 ~ "monolingual",
                                       L1_count == 2 ~ "bilingual",
                                       L1_count > 2 ~ "multilingual")) %>%
      mutate(parent_language = if_else(L1_count == 1, first_language, sample(first_language, size = 1))) %>%
      select(household, parent, parent_language)
    
    # parent_language$parent_language = parent's L1
    parent_language <- merge(parents_L1, children, by = "household")
  }
#### Heidi, Is this the right assumption? Or should each parent speak only a single language to ALL their children? ####
    
  
  # Set years of model run time.
  generation_time = 25
  for(current_year in seq(generation_time)){
   # print(paste("current_year=", current_year, sep = "")) # Loop Counter will appear in the console to let you know how the model run is progressing. 
    agents$year <- max(agents$year) + 1 
    
    #### MAKE AGENTS SPEAK TO EACH OTHER ####
    #### Generate conversation dyadic interactions for each agent ####
    interaction_matrix <- generate_interactions(agents, n_interactions = 10, own_household_prop = household_interaction_prob) 
    # The value for own_household prop = # of non-ego agents in the ego agent's household / total number of non-ego agents. 
    # This should results in an unweighted sampling of all non-ego agents, rather than a household-weighted sample.
    # This means that for each agent, only 1.5% of their interactions will be with members of their own household. Only 1% of their interactions will be with their parents. 
    
    # Now, tally up the full list of conversants/conversations from this matrix. 
    ### Get the interaction list
    interaction_list <- get_interaction_lists(interaction_matrix, agents)
    # count up the number of 'conversations' had by each agent in this year. 
    # interactions_per_agent <- sapply(interaction_list, length)
    
    
    
    #### Each agent chooses the language they speak in each interaction ####
    # This empty list will be populated with the language spoken by each agent in the corresponding position to their agent ID in the interaction_list
    language_of_conversation <- list()

        for(person in 1:length(interaction_list)){  
      focal_agent <- names(interaction_list[person]) # identify focal agent
      # print(focal_agent)
      
      # if(current_year == 1 & focal_agent == "2"){
      #   print("111 Problem")
      # }
    # Identify their family members from the agent trait data frame
      family <- agents[which(agents$household == agents[which(agents$agent_id == focal_agent),]$household & agents$agent_id != focal_agent),]
      parents <- agents[which(agents$household == agents[which(agents$agent_id == focal_agent),]$household & 
                                                                 agents$age > agents[which(agents$agent_id == focal_agent),]$age),]  # identify the focal agent's parents
      sibling <- agents[which(agents$household == agents[which(agents$agent_id == focal_agent),]$household & 
                     agents$age == agents[which(agents$agent_id == focal_agent),]$age &
                       agents$agent_id != as.numeric(focal_agent)),]

      
    # Step 1: Get agent IDs from the named vector single_agent_interactions
      focal_agent_index <- which(agents$agent_id == focal_agent)
      agents_in_interaction <- interaction_list[[person]]
      
    # Step 2: Find the indices of agent IDs in interaction_list[1] that are in the 'parents' data frame
      parent_indices <- which(agents_in_interaction %in% parents$agent_id)
      sibling_indices <- which(agents_in_interaction %in% sibling$agent_id)
    # Step 3: Find the indices of agent IDs in the vector of interactions that are NOT parents (i.e., everyone else)
      other_indices <- which(!(agents_in_interaction %in% c(parents$agent_id, sibling$agent_id)))
      
      
    # Apply Language Choice Rules
    # Step 4: Apply Others Language Choice Rule to non-parent agents: Here, pick at random
      #### Non-parents select one of their known languages at random ####
      if(others_language_choice == "random"){
        agents_in_interaction[sibling_indices] <- select_language_at_random_to_speak(agents_in_interaction[sibling_indices], pop = agents)
        agents_in_interaction[other_indices] <- select_language_at_random_to_speak(agents_in_interaction[other_indices], pop = agents)
      }
      if(others_language_choice == "prestige_A"){
        # see who can speak the prestige language variant.
        other_agents_speak <- agents[which(agents$agent_id %in% agents_in_interaction[other_indices]), ] %>%
          mutate(agent_id = as.numeric(agent_id),
                 language_chosen = case_when(
                   #speak the prestige language if you can.
                   !is.na(`Speaks A`) ~ "Speaks A",
                   # if you can't speak the prestige language, pick one of your known languages at random. 
                   TRUE ~ select_language_at_random_to_speak(agent_id, pop = agents)
                 )) %>%
          select(agent_id, language_chosen)
        
        # Create a data frame for agents in other_indices
        other_agents_df <- data.frame(agent_id = as.numeric(agents_in_interaction[other_indices]))
        
        # Join `other_agents_df` with `tmp` to get the `language_chosen` values for each agent. Agents will sometimes be named more than once in the agents_in_interaction vector. 
        other_agents_df <- other_agents_df %>%
          left_join(other_agents_speak, by = "agent_id")
        
        # Update agents_in_interaction[other_indices] with the matched `language_chosen` values
        agents_in_interaction[other_indices] <- other_agents_df$language_chosen
        
        # the focal agent's sibling still speaks a language chosen at random from their bank of known languages. 
        agents_in_interaction[sibling_indices] <- select_language_at_random_to_speak(agents_in_interaction[sibling_indices], pop = agents)
        
      }
     
      
    # Step 5: Apply Parental Language Choice Rule to Parent Agents: Here, each parent has chosen one of their known languages at random,
    # and they will be consistent about speaking this chosen language to this particular child of theirs. They may speak a different language to their other child. 
     
        #### Parents speak to their children either with a randomly chosen (but consistently used) language, or in the parent's natal language (L1), as specified in the parent_language data frame earlier in the model.  ####
        if(length(parent_indices) > 0){
          agents_in_interaction[parent_indices] <- parent_language[which(parent_language$parent %in% agents_in_interaction[parent_indices] &
                                                                           parent_language$child == as.numeric(focal_agent)),]$parent_language
        } 
      
      # 'agents_in_interaction' is now a vector of the languages spoken by each agents, rather than a vector of the speaking agents' IDs.
      # save this vector in the appropriate spot in a list
      language_of_conversation[[person]] <- agents_in_interaction
    }
    
    
    
#### AGENTS LEARN LANGUAGES FROM THEIR INTERACTIONS #### 
    
    #### Calculate each agent's annual *listening* experience with each language ####  
    ### Tally up agent languages heard, in order to calculate language exposure for focal agents. This will improve their language understanding. 
    annual_listening_experience <- list()
    for(person in 1:length(language_of_conversation)){
      annual_listening_experience[[person]] <- calculate_language_exposures(conversation_languages_vector = language_of_conversation[[person]],
                                                                       pop = agents)
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
    for(i_person in 1:length(languages_spoken)){
      annual_speaking_experience[[i_person]] <- calculate_language_exposures(conversation_languages_vector = language_of_conversation[[i_person]], 
                                                                      pop = agents)
    }  
    annual_speaking_experience <- as.data.frame(do.call(rbind, annual_speaking_experience))
    names(annual_speaking_experience) <- names(agents %>% select(starts_with("Speaks")))
    
    
    
    # calculate agent improvements in language speaking
    ## Note: If you need to adjust the rate of language learning, go change the parameters under the hood for each of these functions. ##
    agents <- learn_languages_by_listening(annual_listening_experience, pop = agents) 
    agents <- learn_languages_by_speaking(annual_speaking_experience, pop = agents)
    
  ### something is wrong with learning by listening....
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


#### The actual model code runs fine, but when I wrap it in a function it starts throwing errors....
return(output)
}


    