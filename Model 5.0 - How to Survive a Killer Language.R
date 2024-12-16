



# How to survive a killer language

### In this world, people learn languages by interacting with other people.
### Two generations are alive at any one time--- a parent generation, and a child generation. Each parent has one child, and each child has one parent. 
### Every 25 years, the parent generation dies and the child generation reproduces and becomes the new parent generation. 
### Agents know whether they are interacting with their parent, their child, or with someone else outside their own household. 
### The proportion of an agent's interactions that are with members of their own household, rather than with members of other households, is a variable in the model.
### When an individual interacts with someone, they choose their language of conversation according to one of several rules:

# - Pick at random from among the languages you know how to speak. 
# - Speak the language your parent taught you (sometimes in the code I use 'L1' to refer to this language)
# - Speak the language in which you have the highest speaking ability. ('best known')
# - Speak the prestige language variant. (which is language A)

set.seed(0)


# Every year, everyone picks their partners (with replacement) for ten conversations. However, because every agent gets this opportunity, an agent is likely to be named among the ten spots of another agent, so almost everyone has more than ten conversations. On average, agents have 22 dyadic conversations a year. 
# Conversation partners do not need to coordinate on a single language within their conversation. Each agent chooses their own language to speak in each conversation independently of their conversation partner's choice. 

# prop_of_intra_household_interactions = a proportion between 0 and 1 that determines the average percent of each agent's conversations that are with the other member of their own household (their parent, if a child; or their child, if a parent), rather than with members of the community outside their own household. 
# parent_language_choice = a character string denoting the language choice rule used by a parent speaking to their child, either "random" or "L1".
# child_language_choice = a character string denoting the language choice rule used by a child speaking to their parent, either "random", "best known", or "L1"
# others_language_choice = a character string denoting the language choice rule used by an agent speaking to any agen who is not their child, either "random", "best known", or "prestige_A". prestige_A means that language A is designated as the prestige language in the simulation. 


prop_of_intra_household_interactions = 0.5
parent_language_choice = "random"
child_language_choice = "random"
others_language_choice = "random"


run_ABM <- function(generations_n, 
                    prop_of_intra_household_interactions,
                    parent_language_choice,
                    child_language_choice,
                    others_language_choice){
  
  #### GENERATE POPULATION DEMOGRAPHY #### 
  # Generate first parent cohort, all age 25, all monolingual in one of five languages (A-E)
  agents <- start_cohort(n = 100, age = 25, n_languages = 3) %>%
    # birth new cohort (children of parent cohort)
     birth_new_cohort()
  # There are now 200 agents: 100 25-year-old parents, and 100 newborns -- one child per parent, sharing a household ID. 

  
  children <- agents[which(agents$generation == max(agents$generation)),] %>%
    select(household, agent_id) %>%
    rename("child" = "agent_id")
  
 
    # Record parents' natal language
    parent_language <- agents[which(!(agents$agent_id %in% children$child)),] %>% select(agent_id, household, age, starts_with("Speaks")) %>%
      pivot_longer(cols = starts_with("Speaks"), values_to = "fluency", names_to = "parent_language") %>%
      filter(fluency>0)  %>%
      rename("parent" = "agent_id") %>%
      select(household, parent, parent_language) %>%
      merge(children, by = "household")
   
  
  
  # Initialize output table
  output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agents)))
  names(output) <- names(agents)
  
  
  # run for generations_n number of generations
  for(g in seq(generations_n)){
    print(paste("generation", g, sep = " ")) # Loop Counter in console will tell you which generation is growing up right now. 
    
    # Set years of model run time.
    generation_time = 4
    for(current_year in seq(generation_time)){
      # print(paste("current_year=", current_year, sep = "")) # Loop Counter will appear in the console to let you know how the model run is progressing. 
      agents$year <- max(agents$year) + 1 
      
      #### MAKE AGENTS SPEAK TO EACH OTHER ####
      #### Generate conversation dyadic interactions for each agent ####
      interaction_matrix <- generate_interactions(agents, n_interactions = 10, own_household_prop = prop_of_intra_household_interactions) 
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
        
        # Identify their parent/child from the agent trait data frame. 
        ## Note: One of these will always be empty, since an agent holds the role of child for the first 25 years of their life and then switches to the role of parent at age 25 when they give birth, at the same time that their parent dies.
        parent <- agents[which(agents$household == agents[which(agents$agent_id == focal_agent),]$household & 
                                  agents$age > agents[which(agents$agent_id == focal_agent),]$age),]  # identify the focal agent's parent
        child <- agents[which(agents$household == agents[which(agents$agent_id == focal_agent),]$household & 
                                agents$age < agents[which(agents$agent_id == focal_agent),]$age),]  # identify the focal agent's child
        
        
        # Step 1: Get agent IDs from the named vector single_agent_interactions
        focal_agent_index <- which(agents$agent_id == focal_agent)
        agents_in_interaction <- interaction_list[[person]]
        
        # Step 2: Find the indices of agent IDs in interaction_list[1] that are in the 'parents' data frame
        parent_indices <- which(agents_in_interaction %in% parent$agent_id)
        child_indices <- which(agents_in_interaction %in% child$agent_id)
        # Step 3: Find the indices of agent IDs in the vector of interactions that are NOT parents (i.e., everyone else)
        other_indices <- which(!(agents_in_interaction %in% c(parent$agent_id, child$agent_id)))
        
        
        # Apply Language Choice Rules
        # Step 4: Apply Others Language Choice Rule to non-parent agents: Here, pick at random
        #### Language Choice Rule for Agents not speaking to their own parent/child ####
        if(others_language_choice == "random"){
          agents_in_interaction[other_indices] <- select_language_at_random_to_speak(agents_in_interaction[other_indices], pop = agents)
        }
        if(others_language_choice == "best_known"){
          agents_in_interaction[other_indices] <- select_language_max_efficacy(agents_in_interaction[other_indices], pop = agents)
        }
        if(others_language_choice == "prestige_A"){
          # see who can speak the prestige language variant.
          other_agents_speak <- agents[which(agents$agent_id %in% agents_in_interaction[other_indices]), ] %>%
            mutate(agent_id = as.numeric(agent_id),
                   language_chosen = case_when(
                     #speak the prestige language if you can.
                     !is.na(`Speaks A`) ~ "Speaks A",
                     # if you can't speak the prestige language, speak the language you know best. 
                     TRUE ~ select_language_max_efficacy(agent_id, pop = agents)
                   )) %>%
            select(agent_id, language_chosen)
          
          # Create a data frame for agents in other_indices
          other_agents_df <- data.frame(agent_id = as.numeric(agents_in_interaction[other_indices]))
          
          # Join `other_agents_df` with `tmp` to get the `language_chosen` values for each agent. Agents will sometimes be named more than once in the agents_in_interaction vector. 
          other_agents_df <- other_agents_df %>%
            left_join(other_agents_speak, by = "agent_id")
          
          # Update agents_in_interaction[other_indices] with the matched `language_chosen` values
          agents_in_interaction[other_indices] <- other_agents_df$language_chosen
        }
        
        
        # Step 5: Apply Parental Language Choice Rule to Parent Agents: 
        #### Language Choice Rule for Parents speaking to their own Children  ####
        if(length(parent_indices) > 0){
          if(parent_language_choice == "random"){
            agents_in_interaction[parent_indices] <- select_language_at_random_to_speak(agents_in_interaction[parent_indices], pop = agents) 
          }
          if(parent_language_choice == "L1"){
            agents_in_interaction[parent_indices] <- parent_language[which(parent_language$parent == parent$agent_id),]$parent_language
          }
          
        }
        
      # Step 6: Apply Child--> parent language choice rule to child agents:
        #### Language Choice Rule for Children speaking to their own Parents ####
        if(length(child_indices) > 0){
          if(child_language_choice == "random"){
            agents_in_interaction[child_indices] <- select_language_at_random_to_speak(agents_in_interaction[child_indices], pop = agents) 
          }
          if(child_language_choice == "best_known"){
            agents_in_interaction[child_indices] <- select_language_max_efficacy(agents_in_interaction[child_indices], pop = agents)
            
          }
          if(child_language_choice == "L1"){
            agents_in_interaction[child_indices] <- parent_language[which(parent_language$child == child$agent_id),]$parent_language
          }
        }
        
        # 'agents_in_interaction' has now been converted to a vector of the languages spoken by each agents, rather than a vector of the speaking agents' IDs.
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
      
      # turn speaker IDs into one long vector for easy indexing
      all_speakers <- interaction_list %>% unname() %>% unlist()
      # do the same with the matching list of languages spoken in each conversation by each speaker
      all_spoken <- language_of_conversation %>% unlist()
      # rename a vector for agent IDs, for ease
      agent_ids <- names(interaction_list)
      
      # I'm sure there's a way to vectorize this for efficiency, but for now...
      languages_spoken <- list()
      for(i in 1:length(agent_ids)){
        speaker_index <- which(all_speakers == agent_ids[i]) # identify the positions at which this agent is named as a speaker
        languages_spoken[[i]] <- all_spoken[speaker_index] # identify the languages they chose to speak on each occasion
        
      }
      
      
      # Now, tally up each agent's experience speaking each language this year. 
      #initialize empty list
      annual_speaking_experience <- list()
      for(i_person in 1:length(languages_spoken)){
        annual_speaking_experience[[i_person]] <- calculate_language_exposures(conversation_languages_vector = languages_spoken[[i_person]], 
                                                                               pop = agents)
      
        }  
      annual_speaking_experience <- as.data.frame(do.call(rbind, annual_speaking_experience))
      names(annual_speaking_experience) <- names(agents %>% select(starts_with("Speaks")))
      
      
      
      # calculate agent improvements in language speaking
      ## Note: If you need to adjust the rate of language learning, go change the parameters under the hood for each of these functions. ##
      agents <- learn_languages_by_listening(annual_listening_experience, pop = agents) 
      agents <- learn_languages_by_speaking(annual_speaking_experience, pop = agents)
      
       
      
      
      # add the census for this year to the running total of data output
      output <- rbind(output, agents)
      
      # everyone gets one year older
      agents$age <- agents$age + 1
      # Repeat all of this living for the next value of time t.
      
      
    }
    
    
    ### In year 26:
    # - The parent generation dies at age 49. 
    # - The child generation becomes the new parent generation, age 25.
    # - A new generation of newborns enters the simulation, age 0. 
    
    
    #### *** Parent natal language and child speaking values in each of the three languages (at age 24? (the age right before children become parents)). 
    
    # children become parents in the parent_language data frame. The parent_language variable stays the same, because the new parents will choose to speak to their children in the language they were taught by their parents. 
    
    # Kill the parent generation
    agents <- agents %>%
      filter(generation == max(generation)) %>%
      # birth new cohort (children of new parent cohort)
      birth_new_cohort()  
    parent_language$parent <- agents[1:100,]$agent_id
    parent_language$child <- agents[101:200,]$agent_id # update the parent_language dataframe with the IDs of the new child generation. 
  }
  
  return(output)
}



###### I BROKE THE LANGUAGE LEARNING FUNCTION. NEED TO FIX!


 test <- run_ABM(generations_n = 2,
                 prop_of_intra_household_interactions = 0.5,
                parent_language_choice = "random",
                child_language_choice = "random",
                others_language_choice = "random")

