


############ TEST FILE ################


# function for effect of age on language learning rate -- THIS WILL CHANGE once I have more information from linguists. 
age_factor <- function(age){
  params <- data.frame(d = 18, a = 0.5, r0 = 9)
  age = as.vector(seq(from = 0, to = 120)) #, mode = "integer", length = 121)
  
tmp <- apply(X = age, FUN = function(i){ params$r0 * (1 - (1 / (1 + exp(-params$a * (age - params$d))))) + 0.5 })




# language_exposures = the output of the calculate_language_exposures() function above, applied to the full agent_census data frame. Should be a data frame with ncols = length(languages) and nrows = nrow(agent_census)
# pop = a data frame of agent characteristics, including agent ID, age, and current proficiency in each language
# pop_languages = a character vector of the names of the languages being simulated in the model space. 
learn_languages <- function(language_exposures = agent_language_exposures, pop = agent_census, pop_languages = languages){
  
  # effect of age on language learning rate -- THIS WILL CHANGE once I have more information from linguists. 
    ages = seq(from = 0, to = 120)
    params <- data.frame(d = 18, a = 0.5, r0 = 9, tc = 0)
    age_factor <-  params$r0 * (1 - (1 / (1 + exp(-params$a * (age - params$tc - params$d))))) + 0.5
    age_rate <- ages * age_factor
    
 for(lang in pop_languages){
      pop[,lang] <- pop[,lang] +  # current language proficiency
        age_rate[pop$age + 1] * language_exposures[lang] # newly gained language proficiency
      
      pop[,lang] <- case_when(pop[,lang] > 100 ~ 100, # set a proficiency ceiling at 100
                              TRUE ~ as.numeric(pop[,lang]))
    }
      
    return(pop)
    
    }

#######################################################################


# agent_conversation_partners = a vector of agent IDs returned by the record_conversations() function above. This function operates on the languages spoken in conversation by the conversation partners of a single agent. Use an apply() function to get a record of conversation languages for all agents. 
# language_choice_strategy = a named function outlining the logic of individual agents for choosing a language to speak in a given interaction. Defaults to random sampling of an agent's known languages. 
# pop = data frame of agent traits. Defaults to the agent_census data frame created by other model functions that should be called before this one. 
agent_conversation_partners = conversants[[1]]
pop = agent_census
select_language_of_conversation_max_proficiency <- function(agent_conversation_partners, pop = agent_census){
  
  # Extract the relevant columns once
  language_data <- pop[pop$agent_id %in% agent_conversation_partners, c ("agent_id", languages)] %>%
    pivot_longer(cols = starts_with("Language"), names_to = "can_speak", values_to = "proficiency") %>%
    filter(proficiency > 20) %>%
    group_by(agent_id) %>%
    summarise(spoken = if(n() > 0) sample(can_speak, size = 1))
  
return(language_data$spoken)
}

select_language_of_conversation_at_random(conversants[[1]])
  



##############################################################################

conversations <- conversants[[1]]
# written to apply to a single row of a matrix
select_language_of_conversation_max_proficiency <- function(conversations, pop = agent_census){
  
  speakers <- pop[pop$agent_id %in% conversations, c ("agent_id", languages)] 
    for(lang in languages){
     speakers[,lang] <- if_else(speakers[,lang] < min_speaking_proficiency, 0, speakers[,lang])
    }
  
  
  speakers  <- speakers %>%
    rowwise() %>%
    mutate(
      max_proficiency = max(c_across(starts_with("Language")), na.rm = TRUE),
      highest_proficiency_languages = list(names(.[,-1])[which(c_across(starts_with("Language")) == max_proficiency)]),
      preferred_language = if (max_proficiency < 20) {
        NA
      } else {
        max_langs <- names(.[,-1])[which(c_across(starts_with("Language")) == max_proficiency)]
        if (length(max_langs) == 1) {
          max_langs
        } else {
          sample(max_langs, 1)
        }
      }
    ) %>%
    ungroup() 
  
  
  prof_match <- data.frame(agent_id = speakers$agent_id[-1])
  for (lang in languages) {
    if (speakers[1, lang] != 0) {
      prof_match[, lang] <- if_else(as.numeric(speakers[1, lang]) < speakers[-1, lang], speakers[1, lang], speakers[-1, lang])
    } else { 
      prof_match[, lang] <- NA
    }
  }
  prof_match[prof_match == 0] <- NA
  prof_match$preferred_language <- speakers$preferred_language[-1]
  
  prof_match <- prof_match %>%
    rowwise %>%
    mutate(
    max_proficiency = max(c_across(starts_with("Language")), na.rm = TRUE),
    highest_proficiency_languages = list(names(.[,-1])[which(c_across(starts_with("Language")) == max_proficiency)]),
    language_of_conversation = if(length(highest_proficiency_languages) > 0){sample(highest_proficiency_languages, 1)} else{
      preferred_language
    }) %>%
    ungroup()
language_of_conversation <- prof_match$language_of_conversation
  
  return(language_of_conversation)

}


test <- select_language_of_conversation_max_proficiency(conversations = conversants[[7]])
print(test)
###########################################################################




      # this means if the other agent cannot speak this language, the proficiency value will be NA.
      # if the other agent speaks this language better than the focal agent, the proficiency value will be the focal agent's value.
      # if the focal agent speaks this language better than their dyadic partner, the proficiency value will be their partner's value.
   # the proficiency value is NA if the focal agent doesn't speak the language. 
  
   max_col_names <- apply(prof_match[languages], 1, function(row) {
        if (sum(!is.na(row)) > 0) { # If the conversational dyad shares at least 1 language
         max_val <- max(row, na.rm = TRUE) # identify the max value for the min proficiency in a shared language
         max_indices <- which(row == max_val) # Indices of columns with max value
         if (length(max_indices) > 1) {    # If min. proficiency is tied for more than one language       
           sample(languages[max_indices], 1) # Randomly select one for the agents to speak in
           } else {
             languages[max_indices] # Otherwise, pick the language with the highest minimum proficiency (e.g., Your English is better than my German, so let's speak English)
           }
         } else{ prof_match$preferred_language[row] } # If the agents don't speak any languages in common, each agent defaults to their preferred language (NA if infant)
     })
   
   
   
   max_col_names <- apply(prof_match[languages], 1, function(row) {
     if (sum(!is.na(row)) > 0) {
       max_val <- max(row, na.rm = TRUE)
       max_indices <- which(row == max_val)
       if (length(max_indices) > 1) {
         sample(languages[max_indices], 1)
       } else {
         languages[max_indices]
       }
     } else {
       speakers$preferred_language[row]
     }
   })
   
   
   if(is.na(max_col_names)){ # If the agent doesn't speak any languages
     if (sum(!is.na(speakers[row, languages])) == 0) { NA } else{
     if (sum(!is.na(speakers[row, languages])) > 0) { # If the agent speaks at least 1 language
       max_val <- max(speakers[row, languages], na.rm = TRUE) # identify the max value for the min proficiency in a shared language
       max_indices <- which(row == max_val) # Indices of columns with max value
       if (sum(speakers[which(speakers[row, languages] == max_val)]) > 1) {    # If min. proficiency is tied for more than one language       
         sample(languages[max_indices], 1) # Randomly select one for the agent to speak in
       } else {
         languages[max_indices] # Otherwise, pick the language with the highest  proficiency 
       }
     # If both agents can speak but don't share a language, then each agent speaks at the other agent in their preferred language (language of max proficiency)
     }
     }
   }
}




max_col_names <- apply(prof_match[languages], 1, function(row, row_index) {
  if (sum(!is.na(row)) > 0) {
    max_val <- max(row, na.rm = TRUE)
    max_indices <- which(row == max_val)
    if (length(max_indices) > 1) {
      sample(languages[max_indices], 1)
    } else {
      languages[max_indices]
    }
  } else {
    agent_id <- prof_match$agent_id[row_index]
    speakers$preferred_language[speakers$agent_id == agent_id]
  }
}, row_index = seq_len(nrow(prof_match)))

   return(max_col_names)
})
   
           
test <- select_language_of_conversation_max_proficiency(conversants[[1]], languages = languages)
test


#### Function to select the language of conversation: Agents must agree on which language to speak. 
# They select the language that maximizes shared proficiency
# To do this, compare conversation partners' proficiency values for each language. Pick the language that has the highest low value. 

# converse_in_max_proficiency <- function(agent_conversation_partners, pop = agent_census){
#
#   # Extract the relevant columns
#   language_data <- pop %>%
#     select(agent_id, contains("Language"))
#
#   ego <- agent_conversation_partners[1]
#   alters <- agent_conversation_partners[-1]
#
#   spoken_ego <- vector("character", length(agent_conversation_partners))
#   spoken_alter <- vector("character", length(agent_conversation_partners))
#
#

# calc_highest_min_proficiency <- function(alter){
#   for(lang in languages){ # for each language, return the min proficiency value in the conversant dyad.
#    min_proficiency <- min(c(language_data[1, lang], language_data[which(language_data$agent_id == alter), lang]))
#    if(min_proficiency > 20){ # but only if both partners have a proficiency value in this language > a monolingual two-year-old
#      lang_match[lang] <- min_proficiency}
#    else(lang_match[lang] <- max(c(language_data[1, lang], language_data[which(language_data$agent_id == alter), lang]))) # if at least one partner is an infant, select the strongest language of the other partner.
#      names(lang_match[lang]) <- languages[lang]
#
#   }
# converse_in <- names(lang_match[which(lang_match == max(lang_match))]) # list the names of the agents' shared languages with the highest min proficiency value
# if(length(converse_in) > 1) converse_in <- sample(converse_in, size = 1) # if there's a tie among conversation partners for the language with the highest min proficiency, pick amongt the tied languages at random.
# converse_in <- converse_in[!is.na(converse_in)]  # remove NAs (conversation partner was an infant who can't speak yet)
# return(converse_in)
# }
#


#test <- sapply(alters, FUN = calc_highest_min_proficiency)


# conversations = a character vector of agent IDs (like one of the vectors stored in the list 'conversants').
# pop = agent_census, a data frame of agents and their traits
# languages = a character vector naming the languages that exist in this simulation.
# min_speaking_proficiency = a number (defaults to 1) that sets the proficiency threshold an agent must reach in a language in order to speak that language in an interaction with another agent. 

select_language_of_conversation_max_proficiency <- function(conversations, pop = agent_census, languages, min_speaking_proficiency = 1) {
  
  speakers <- pop[pop$agent_id %in% conversations, c("agent_id", languages)]
  
  for (lang in languages) {
    speakers[, lang] <- if_else(speakers[, lang] < min_speaking_proficiency, NA, speakers[, lang])
  }
  
  # Determine preferred language (language of max proficiency) for each speaker
  asymmetric_exchanges <- speakers[-1,] %>%
    rowwise() %>%
    mutate(max_proficiency = max(c_across(all_of(languages)), na.rm = TRUE),
           preferred_languages = list(names(speakers[languages])[which(c_across(all_of(languages)) == max_proficiency)]),
           preferred_language = NA)
  for(i in 1:nrow(asymmetric_exchanges)){
    if(length(asymmetric_exchanges$preferred_languages[[i]]) > 0){
 asymmetric_exchanges$preferred_language[i] = sample(asymmetric_exchanges$preferred_languages[[i]], 1)
    }
    else{NA}
  }
          
           
prof_match <- data.frame(agent_id = speakers$agent_id[-1])
  
  for (lang in languages) {
    if (!is.na(speakers[1, lang])) {
      prof_match[, lang] <- if_else(speakers[1, lang] < speakers[-1, lang], speakers[1, lang], speakers[-1, lang])
    } else {
      prof_match[, lang] <- NA
    }
  }
  
max_col_names <- apply(prof_match[languages], 1, function(row) {
    if (sum(!is.na(row)) > 0) { # if both agents share at least one language
      max_val <- max(row, na.rm = TRUE) # identify the language(s) with the maximum min proficiency between these agents
      max_indices <- which(row == max_val)
      if (length(max_indices) > 1) { # if their language of shared max proficiency is a tie,
        sample(languages[max_indices], 1) #
      } else {
        languages[max_indices] # if there's only option, that's the language they speak
      }
    } else { #If agents don't share any languages, the preferred_language of the alter agent (other agent in the interaction) is sampled.
      NA
  }
    })

asymmetric_exchanges$max_col_names <- max_col_names

asymmetric_exchanges <- asymmetric_exchanges %>%
  mutate(language_of_conversation = if_else(!is.na(max_col_names), max_col_names, preferred_language)) 
  
  return(asymmetric_exchanges$language_of_conversation)
}





test <- select_language_of_conversation_max_proficiency(interactions[4,], pop = agent_census, languages, min_speaking_proficiency = 20)





##### TEST RUN TIME ####
start.time <- Sys.time()

# Your R code here
initial_ages <- generate_age_structure(n = 10000, mortality = CDW15, years = 300)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken



