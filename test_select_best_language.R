
#### Select Language of greatest speaking ability #### 
# Function to choose the language with the highest speaking value as each agent's language to speak in a conversation. If an agent's highest speaking value is tied across multiple languages, sample the tied languages at random. 
# conversations =  a vector of agent IDs, probably from the interactions_list() of dyadic conversation partners
# pop = the main active data frame of agent attributes.

# select_language_max_efficacy <- function(conversations, pop) {
#   
#   # identify the languages in the simulation space
#   speaks <- names(pop %>% select(starts_with("Speaks")))
#   understands <- names(pop %>% select(starts_with("Understands")))
#   
#   #extract degree of language understanding for each agent interacting with focal agent
#   understands_data <- pop[match(agents_to_speak, pop$agent_id), c("agent_id", understands)]
#   speaks_data <- pop[match(agents_to_speak, pop$agent_id), c("agent_id", speaks)]
#   for(language in seq(speaks){
#     if(understands_data)
#   }
#   language_data[,speaks] <- case_when(language_data[,"Understands A"])
#   
#   
#   #calculate sampling weights for each agent choosing the language (this will be 0 or 1)
#   language_weights = matrix(0, nrow=nrow(language_data), ncol=length(speaks))
#   
#   
#   has_speech = rowSums(language_data[, understands]) > 15 #vector of true/false, corresponding to agents_to_speak (and to rows in language_weights)
#   language_weights[has_speech] <- apply(language_data[has_speech, speaks], 1, function(x) speaks[which(x == max(x, na.rm = T))])
#   
#   
#   
#   
#   #Sample spoken language, or use "none" if speechless
#   spoken = rep("none", length(agents_to_speak)) #Replace "none" with whatever you want speechless agents to use
#   if (any(has_speech)) { #only sample languages if at least one interacting agent has speech
#     spoken[has_speech] = apply(language_weights[has_speech,,drop=FALSE], 1, function(probs) {paste0("Speaks ", 
#                                                                                                     sample(str_sub(understands, -1, -1), 1, 
#                                                                                                            prob=probs))})
#   }
#   language_weights[language_data[,speaks] == max(speaks, na.rm. = T)] = 1
#   # Subset the speaking values for the agents named in 'conversations'
#   speakers <- pop[pop$agent_id %in% conversations, c("agent_id", languages)]
#   
#   # Identify rows where agents know at least one language 
#   speakers_indices <- which(speakers$agent_id %in% speakers[rowSums(!is.na(speakers[, languages])) > 0, ]$agent_id)
#   #  NA_indices <- which(!speakers$agent_id %in% speakers[rowSums(!is.na(speakers[, languages])) > 0, ]$agent_id)
#   
#   # Identify the languages with the highest speaking values for each agent
#   highest_proficiency_languages <- vector("list", length = length(conversations))
#   highest_proficiency_languages[speakers_indices] <- apply(speakers[speakers_indices, languages], 1, function(x) languages[which(x == max(x, na.rm = TRUE))])
#   
#   # Determine the preferred language for each agent
#   preferred_language <- sapply(1:nrow(speakers), function(x) {
#     if (length(highest_proficiency_languages[[x]]) < 1) {
#       return(NA)
#     } else if (length(highest_proficiency_languages[[x]]) == 1) {
#       return(highest_proficiency_languages[[x]])
#     } else {
#       # If multiple languages are tied for the highest value, sample from these at random
#       return(sample(highest_proficiency_languages[[x]], 1))
#     }
#   })
#   
#   return(preferred_language)
# }
# 










select_best_language = function(agents_to_speak, pop) {
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
    
  }
    
  return(spoken)
}



# If an agent can only speak a language they've already spoken before, then this rule will stop anyone from learning to speak any new languages. 


agents_to_speak = c(1, 3, 1, 4, 5, 6, 1)
#agents_to_speak = c(5, 6)
pop = data.frame(agent_id=seq(1, 6),
                 "Understands A"=c(0, 0, 0, 0, 0, 5),
                 "Understands B"=c(30, 30, 30, 0, 0, 0),
                 "Understands C"=c(0, 0, 100, 0, 0, 0),
                 "Speaks A" = c(0, 0, 0, 0, 0, 0),
                 "Speaks B" = c(10, 5, 0, 0, 0, 0),
                 "Speaks C" = c(0, 0, 20, 0, 0, 0)
)
names(pop) = c("agent_id", "Understands A", "Understands B", "Understands C", "Speaks A", "Speaks B", "Speaks C")

spoken = select_best_language(agents_to_speak, pop)
print(spoken)




