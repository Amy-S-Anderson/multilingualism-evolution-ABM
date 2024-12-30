




### if parent_language only contains the household and the name of the language that the grandparent generation spoke to the generation who are now parents?

# 1. What is your households heritage language
# 2. can you speak it?
# 3. How well can you speak it?
# agents_to_speak = interaction_list[[35]]
# pop = agents
# threshold_of_ability = 20




# agents_to_speak = vector of agent IDs
# pop = data frame of agents (rows) and their traits (columns)
# heritage_language = data frame with household IDs, and the heritage language for each household (determined when 1st generation agents are generated)
# threshold_of_ability = a number between 0 and 100. The speaking value above which an agent *can* choose to speak their heritage language. This is likely to be quite high for parents choosing whether to speak this language to their children, but can be 0 for children just beginning to learn the language. 


# heritage language = a data frame of two columns: household ID and name of that household's heritage language (the language assigned to the monolingual ancestor in the immaculate conception generation)
select_heritage_language <- function(agents_to_speak, pop, heritage_language, threshold_of_ability){
  #get column names for degree of language understood
  understands <- names(pop)[which(startsWith(names(pop), "Understands"))]
  #get column names for degree of language spoken
  speaks <- names(pop %>% select(starts_with("Speaks")))
  
  #extract degree of language understanding for each agent interacting with focal agent
  understanding_data <- pop %>% 
    filter(agent_id %in% agents_to_speak) %>%
    select(agent_id, all_of(understands)) %>%
    pivot_longer(cols = understands, names_to = "understands", values_to = "skill_level") %>%
    filter(skill_level > (100/12) * 2) # retains only rows with data on a language that an agent understands well enough to speak it.
  
  # extract each agent's degree of speaking skill in each language.
    speaking_data <- pop %>% 
    filter(agent_id %in% understanding_data$agent_id) %>%
    select(agent_id, all_of(speaks), household) %>%
    pivot_longer(cols = speaks, names_to = "speaks", values_to = "speaking_level") %>%
    group_by(agent_id) %>%
      # identify which language is their best known language (and if languages are tied for best known, pick one of them)
    mutate(best_known = sample(speaks[which.max(speaking_level)], 1))
  
  #identify the heritage language in each household
  heritage = pop %>%
    filter(agent_id %in% understanding_data$agent_id) %>%
    merge(heritage_language, by = "household") %>%
    select(agent_id, L1)
  
  # identify hertiage language speakers in the agents_to_speak vector
  speaker_options <- speaking_data %>%
    merge(heritage_language, by = "household") %>%
    group_by(agent_id) %>%
    mutate(heritage_speaker = if_else(any(speaks == L1 & speaking_level > threshold_of_ability),"yes", "no")) %>%
    select(agent_id, best_known, heritage_speaker, L1) %>%
    distinct() %>%
    mutate(spoken = if_else(heritage_speaker == "yes", L1, best_known)) %>%
    select(agent_id, spoken) 
  
  
  has_speech <- which(agents_to_speak %in% understanding_data$agent_id)  # Vector of true/false, corresponding to agents_to_speak
  # Match the chosen language to the agents_to_speak
  spoken <- rep("none", length(agents_to_speak))
  if (nrow(speaker_options) > 0) {  # Only assign languages if at least one agent has a valid choice
    # Match agents_to_speak with their chosen language
    chosen_languages <- speaker_options$spoken[match(agents_to_speak[has_speech], speaker_options$agent_id)]
    
    # Assign chosen languages to the respective positions in the spoken vector
    spoken[has_speech] <- chosen_languages
  }
  
  return(spoken)
}
  

   
   
   
   
   
   
   
   
   #get column names for degree of language understood
   understands <- names(pop)[which(startsWith(names(pop), "Understands"))]
   #get column names for degree of language spoken
   speaks <- names(pop %>% select(starts_with("Speaks")))
   
   #extract degree of language understanding for each agent interacting with focal agent
   understanding_data <- pop %>% 
     filter(agent_id %in% agents_to_speak) %>%
     select(agent_id, all_of(understands)) %>%
     pivot_longer(cols = understands, names_to = "understands", values_to = "skill_level")
   
   speaking_data <- pop %>% 
     filter(agent_id %in% agents_to_speak) %>%
     select(agent_id, all_of(speaks)) %>%
     pivot_longer(cols = speaks, names_to = "speaks", values_to = "speaking_level")
   
   language_data <- merge(understanding_data, speaking_data, by = "agent_id") %>%
     filter(skill_level > 15) %>%
     group_by(agent_id) %>%
     filter(speaking_level == max(speaking_level, na.rm = T)) %>%
     mutate(chosen_language = sample(speaks, size = 1)) %>%
     select(agent_id, chosen_language)
   
   spoken = rep("none", length(agents_to_speak))
   has_speech = which(agents_to_speak %in% language_data$agent_id) #vector of true/false, corresponding to agents_to_speak
   if(nrow(language_data) > 0){   #only sample languages if at least one interacting agent has speech
     # Match agents_to_speak with their chosen language
     chosen_languages <- language_data$chosen_language[match(agents_to_speak[has_speech], language_data$agent_id)]
     
     # Assign chosen languages to the respective positions in the spoken vector
     spoken[has_speech] <- chosen_languages
     
  
  
  %in% parent_language$household) %>%
    left_join(pare)
  
  able_to_speak <- c(NA, length(agents_to_speak))
  
  which(agents_to_speak[(pop[agents_to_speak]$age == max(pop$age))])
  # If the agents are parents
  if(pop[agents_to_speak]$age == max(pop$age)){
    able_to_speak
    
  }
  able_to_speak <- parent_language[which(parent_languageagents_to_speak]
  #extract degree of language understanding for each agent interacting with focal agent
  understanding_data <- pop %>% 
    filter(agent_id %in% agents_to_speak) %>%
    select(agent_id, all_of(understands)) %>%
    pivot_longer(cols = understands, names_to = "understands", values_to = "skill_level")
  
}





agents[which(agents$agent_id %in% agents_in_interaction[other_indices]), ] %>%
  mutate(agent_id = as.numeric(agent_id),
         language_chosen = case_when(
           #speak the prestige language if you can.
           !is.na(`Speaks A`) ~ "Speaks A",
           # if you can't speak the prestige language, speak the language you know best. 
           TRUE ~ select_best_language(agent_id, pop = agents)
         )) %>%
  select(agent_id, language_chosen)
