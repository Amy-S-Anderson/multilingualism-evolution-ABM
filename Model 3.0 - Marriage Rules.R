

### Marriage Rules and Multilingualism ###

# Marry at Random with regard to partner's Languages
# Half of interactions are with members of the same household
# Parents choose one of their known languages at random to speak to their children
# Other agents also choose one of their known languages at random to speak to their children

#### GENERATE POPULATION DEMOGRAPHY #### 
# Generate first parent cohort, all age 25, all monolingual in one of five languages (A-E)
  agents <- start_cohort(n = 100, age = 25, n_languages = 5)
# Record first gen's first language (they're monolingual, so it's the same as the only language they speak)
  first_language <- agents %>% select(agent_id, starts_with("Speaks")) %>%
    pivot_longer(cols = starts_with("Speaks"), values_to = "fluency", names_to = "first_language") %>%
    filter(!is.na(fluency)) %>%
    select(agent_id, first_language) 
 # agents <- cbind(agents, first_language)
  agents$generation <- 0
    
# Marry off the first cohort to each other, at random. 
  agents <- marry_random(agents) %>%
# birth new cohort (children of parent cohort)
  birth_new_cohort()
  
  

  
  
  # Initialize output table

  output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agents)))
  names(output) <- names(agents)
  
  # set number of generations to run
  generations = 10
  for(g in seq(generations)){
    print(paste("generation", g, sep = " ")) # Loop Counter in console will tell you which generation is growing up right now. 
    
  # Set years of model run time.
  tmax = 25

  for(i in seq(tmax)){
    print(i) # Loop Counter will appear in the console to let you know how the model run is progressing. 
  
  agents$year <- max(agents$year) + 1 
  
#### MAKE AGENTS SPEAK TO EACH OTHER ####
  
#### Generate conversation dyadic interactions for each agent ####
  interaction_matrix <- generate_interactions(agents, n_interactions = 10, own_household_prop = 0.5) 
  # Now, tally up the full list of conversants/conversations from this matrix. 
  ### Get the interaction list
  interaction_list <- get_interaction_lists(interaction_matrix, agents)
  # count up the number of 'conversations' had by each agent in this year. 
 # interactions_per_agent <- sapply(interaction_list, length)
  
  
  
#### Each agent chooses the language they speak in each interaction ####
  # This empty list will be populated with the language spoken by each agent in the corresponding position to their agent ID in the interaction_list
  language_of_conversation <- list()

    for(i in 1:length(interaction_list)){  
    focal_agent <- names(interaction_list[i]) # identify focal agent
    # Identify their family members from the agent trait data frame
    family <- agents[which(agents$household == agents[which(agent_id == focal_agent),]$household & agents$agent_id != focal_agent),]
    parents <- family[which(family$age > agents[which(agent_id == focal_agent),]$age),]  # identify the focal agent's parents
    
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
    
    # Step 5: Apply Parental Language Choice Rule to Parent Agents: Here, speak L1, the first language learned. 
#### Parents speaking to their children speak using their L1 ####
    if(length(parent_indices) > 0){
        # Create a corresponding vector of first_language values from 'parents' for those indices
        # Populate list_of_vectors for interaction_list[1] at the corresponding positions
        agents_in_interaction[which(agents_in_interaction %in% parents$agent_id)] <- first_language$first_language[match(agents_in_interaction[parent_indices], first_language$agent_id)]
       #   parents$first_language[match(agents_in_interaction[parent_indices], parents$agent_id)]
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
  
  
  
  
  
  
  ############################# Model output #################################
  
  
  count_speakers <- function(output, efficacy_threshold){
    speaker_count_table <- output %>%
      group_by(year, generation) %>%
      summarise(A = round((sum(`Speaks A` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
                B = round((sum(`Speaks B` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
                C = round((sum(`Speaks C` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
                D = round((sum(`Speaks D` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
                E = round((sum(`Speaks E` >= efficacy_threshold, na.rm = T) / n() * 100), 2))
    return(speaker_count_table)
  }
  
  speaker_count_table20 <- count_speakers(output, efficacy_threshold = 20)
  speaker_count_table100 <- count_speakers(output, efficacy_threshold = 100)
  
  
  # transform speaker frequency table into long data for plotting
  speaker_freq20 <- speaker_count_table20 %>%
    pivot_longer(cols = c("A", "B", "C", "D", "E"), names_to = "Language", values_to = "Speakers")
  speaker_freq100 <- speaker_count_table100 %>%
    pivot_longer(cols = c("A", "B", "C", "D", "E"), names_to = "Language", values_to = "Speakers")
  
  #### Plot: % of Population with speaking ability in each language ####
  plot_grid(
    ggplot(speaker_freq20, aes(x = year, y = Speakers)) +
    geom_line(aes(color = as.factor(generation))) +
    facet_wrap(~Language, ncol = length(unique(speaker_freq20$Language))) +
    theme_linedraw() +
    labs(y = "% of population") +
    ggtitle("% with any ability to speak") +
    theme(legend.position = "none"),
    ggplot(speaker_freq100, aes(x = year, y = Speakers)) +
      geom_line(aes(color = as.factor(generation))) +
      facet_wrap(~Language, ncol = length(unique(speaker_freq20$Language))) +
      theme_linedraw() +
      labs(y = "% of population") +
      ggtitle("% with 100% speaking efficacy"),
      #theme(legend.position = "none"),
    ncol = 1)
  
  
  
  
  
  
  # make data long data for plotting.
  longdata_speaks <- output %>%
    pivot_longer(cols = starts_with("Speaks"), names_to = "Language", values_to = "Efficacy")
  longdata_understands <- output %>%
    pivot_longer(cols = starts_with("Understands"), names_to = "Language", values_to = "Efficacy")
  
  plot_proficiency_trajectories <- function(longdata){
   # subset <- round(seq(from = 1, to = max(longdata$year), by = (max(longdata$year) / 4)))
    
    ggplot(longdata, aes(x = year, y = Efficacy)) +
      geom_line(aes(group = agent_id, color = as.factor(generation)), alpha = 0.25) +
      facet_wrap(~Language, ncol = 1) +
      labs(color = "Generation") +
      theme_bw()
  }
  
  #### Plot: Speaking/Understanding Trajectories over time of model run. ####
  plot_grid(
    plot_proficiency_trajectories(longdata_speaks),
    plot_proficiency_trajectories(longdata_understands),
    ncol = 2
  )
  
  
  
  
  plot_bio_samples <- function(longdata_speaks, longdata_understands){
    sub.num <- round(seq(from = 1, to = length(unique(longdata_speaks$agent_id)), by = (length(unique(longdata_speaks$agent_id)) / 12)))
    sub <- paste("id_", sub.num, sep = "")
    
    plotdata_speaks <- longdata_speaks[which(longdata_speaks$agent_id %in% sub),] %>%
      mutate(agent_id = factor(agent_id, levels = sub))
    plotdata_understands <- longdata_understands[which(longdata_understands$agent_id %in% sub),] %>%
      mutate(agent_id = factor(agent_id, levels = sub))
    
    
    ggplot(plotdata_speaks, aes(x = age, y = Efficacy)) +
      geom_line(aes(color = Language)) +
      geom_line(data = plotdata_understands, aes(x = age, y = Efficacy, color = Language), linetype = "dashed") +
      facet_wrap(~ agent_id) +
      theme_bw()
  }  
  
  
  plot_bio_samples(longdata_speaks, longdata_understands)
  # right now this looks a mess because the acquisition rate for understanding is the same as the acquisition rate for speaking. 
  
  
  
  
  
  #### Experiments to Run ####
  
  # Parameter Sweep on weighted probability of within-household interactions
  # Parameter Sweep on adherence to Marriage Rules. 
  # Marriage Rules: Right now they marry at random. 
  # How do differences in mutual intelligibility affect outcomes?
  
  
  
  #### Things to do ####
  
  # Make a plot function for plotting child language trajectories grouped by parent speaking choices. 
  # Make function: choose to speak language of highest speaking efficacy
  # Make function: Marry based on language rules.
  # Setup: Mutual Intelligibility Parameter. 
  # Setup: Faster language acquisition for L3+ in children.
  # Adjust Language Acqusition Rate for both speaking and understanding
  # Add a Forgetting Option
  
  # Make a 3-generation version of the model?
  
  
  
  #### I think that's enough for today. 
 
  
  # replace agent IDs with agent languages spoken.  <--- you already have code for this.
  # tally up agent languages heard. <--- you already have code for this.
  # tally up the languages spoken by each agent. because speaking only improves by speaking. 
  # calculate agent improvements in language speaking
  # calculate agent improvements in language understanding
  
  
  
  

 