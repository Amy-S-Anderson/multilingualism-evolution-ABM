


############ TEST FILE ################


####  - Designate languages in play. ####
languages <- choose_local_languages(3)

#### - Designate the mortality hazard for the population ####
# choose the Siler model parameter values for the mortality regime your agents will experience.
CDW15 <- data.frame(a1 = 0.175, b1 = 1.4, a2 = 0.00368, a3 = 0.000075, b3 = 0.0917)

####  - Generate a starting set of agents, and their age structure ####
initial_ages <- read.csv("starting_age_structure.csv")
agent_census <- make_basic_population(n_agents = 1000, age_structure = initial_ages$initial_ages) %>%
  assign_starting_proficiency(languages = languages) %>%
  mutate(place_id = c(rep(1,500), rep(2,500)))



### Assign initial languages proficiencies for agents at Time 0
# This function assumes an equal number of monolingual speakers for each local language and determines language proficiency by agent age.
test_census <- assign_starting_proficiency(agent_census)
write.csv(test_census, file = "starting_agents_for_test_file.csv")





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
# conversations = a vector of agent IDs in which the first entry is the focal agent and the following entries are all the agents who conversed with them in this round.
# pop = agent_census, a data frame of agent IDs and agent characteristics, including proficiency values for each language in the simulation
# languages = a global object, a character vector naming the languages at play in the simulation. 
select_language_of_conversation_max_proficiency <- function(conversations, pop = agent_census){
  
  # set up speakers and their language proficiencies
  speakers <- pop[pop$agent_id %in% conversations, c ("agent_id", languages)] %>% # subset the language proficiencies for the agents named in 'conversations'
    rowwise() %>% # for each agent
    # identify their max proficiency value
    mutate(max_proficiency = max(c_across(starts_with("Language")), na.rm = TRUE), 
      # pull the names of the languages for which their proficiency value = their max proficiency (may be more than one language)
           highest_proficiency_languages = list(names(.[,-1])[which(c_across(starts_with("Language")) == max_proficiency)]),
      # identify the agent's preferred language
           preferred_language = if (max_proficiency < min_speaking_proficiency) { # they can't speak anything if they haven't passed the min_speaking_proficiency threshold
        NA
      } else {
        if (length(highest_proficiency_languages) == 1) {
          highest_proficiency_languages
        } else {
          sample(highest_proficiency_languages, 1)
        }
      }
    ) %>%
    ungroup() 
  
  prof_match <- data.frame(agent_id = speakers$agent_id[-1]) # subset everyone but the focal agent
  for (lang in languages) { # for each language
    prof_match[, lang] <- if(speakers[1, lang] > min_speaking_proficiency){
      if_else(speakers[-1, lang] > min_speaking_proficiency,  # if they can both speak the language
      # replace their individual proficiency value with the minimum of c(their value, the focal agent's value)
      if_else(as.numeric(speakers[1, lang]) < speakers[-1, lang], speakers[1, lang], speakers[-1, lang]),
      NA) # if they don't speak the language, they can't have a matched min proficiency
    } else { NA } }
      
  prof_match <- prof_match %>%
    mutate(preferred_language = speakers$preferred_language[-1]) %>%
    rowwise %>% # for each agent
    # use the same logic as above in 'speakers' to identify the shared language that maximizes the minimum proficiency in each dyad
    mutate(max_proficiency = max(c_across(starts_with("Language")), na.rm = TRUE),
    highest_proficiency_languages = list(names(.[,-1])[which(c_across(starts_with("Language")) == max_proficiency)]),
    # if the dyad doesn't speak any languages in common, each agent speaks their own preferred language (language of highest proficiency)
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

############################################################################



sow_stationary <- function(n_births, agent_census){
  fertile_myrtles <- subset(agent_census, female == 1 & 
                              age >= 15 & age <= 49 &
                              !is.na(spouse_id) &
                              is.na(death_recorded))
  # average annual probability of giving birth
  # This equation determines the individual-level probability of giving birth based on the total number of fertile women and the total number of births needed to balance deaths and maintain population stationarity.
  #  annual_birth_probability <- n_births / nrow(fertile_myrtles) 
  if(nrow(fertile_myrtles) >= n_births){
    new_moms <- data.frame(agent_id = sample(fertile_myrtles$agent_id, size = n_births, replace = F)) 
    # If the number of children that must be born to maintain population stationarity is larger than the number of potential mothers, allow births of twins, triplets, etc. 
  } else new_moms <- data.frame(agent_id = sample(fertile_myrtles$agent_id, size = n_births, replace = T)) 
  
  new_parents <- left_join(new_moms, fertile_myrtles[,c("agent_id", "spouse_id")], by = "agent_id")
  
  return(new_parents)
}

test <- agent_census[which(agent_id %in% c())]

### Adding a place_ID assignment to the birth_new_agents() function
birth_new_agents <- function(agent_census, new_parents){
  if(nrow(new_parents) > 0){
    # Create a data frame with a single row of NA values
    newborns <- data.frame(matrix(0, nrow = nrow(new_parents), ncol = ncol(agent_census)))
    # Set the column names to match those of agent_census
    colnames(newborns) <- colnames(agent_census)
    
    newborns$agent_id <- sapply(seq(from = max(as.numeric(substr(agent_census$agent_id, 4, nchar(agent_census$agent_id)))),
                                    length.out = nrow(new_parents)), 
                                generate_agent_id)
    newborns$age <- 0
    newborns$female <- sample(c(0,1), size = nrow(new_parents), replace = T)
    newborns$spouse_id <- NA
    newborns$mother_id <- new_parents$agent_id
    newborns$father_id <- new_parents$spouse_id
    newborns$death_recorded <- NA
    if(any(names(agent_census) %in% "place_id")){
    newborns$mom_place_id <- agent_census[which(agent_census$agent_id %in% new_parents$agent_id),]$place_id
    newborns$dad_place_id <- agent_census[which(agent_census$agent_id %in% new_parents$spouse_id),]$place_id
    newborns <- newborns %>%
       rowwise %>%
       mutate(
         place_id = c_across(sample(c("mom_place_id", "dad_place_id"), 1))) %>%
      select(-mom_place_id, -dad_place_id)
      
    }
    agent_census <- rbind(agent_census, newborns)
  }
  return(agent_census)
}






calc_dyad_age_and_place <- function(single_women, single_men, woman, man){
  
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  place_compatibility <- single_men$place_id[man] - single_women$place_id[woman]
  # dyad compatibility = sum of the absolute value of the two agents' difference in age + abs value of difference in two agent's place ID. 
  dyad_score <- abs(age_gap) + abs(place_compatibility) 
  return(dyad_score)
}

dyad_scores <- matrix(NA, nrow = nrow(single_women), ncol = nrow(single_men))

for(woman in 1:nrow(single_women)){
  for(man in 1:nrow(single_men)){
    
    dyad_scores[woman,man] <- calc_dyad_age_and_place(single_women, single_men, woman, man)
  }
}
rownames(dyad_scores) <- single_women$agent_id
colnames(dyad_scores) <- single_men$agent_id



####### THIS FUNCTION DOESN'T WORK YET -- NEEDS PROFICIENCY SCORES IN THE LANGUAGE COLUMNS IN ORDER TO TEST IT. 
calc_dyad_age_language_max <- function(single_women, single_men, woman, man, proficiency_threshold = 100){
  
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  woman_max_proficiency = languages[which(single_women[woman, languages] == max(single_women[woman, languages]))]
  man_max_proficiency = languages[which(single_men[man, languages] == max(single_men[man, languages]))]
  
 if(woman_max_proficiency %in% man_max_proficiency){
   dyad_score <- abs(age_gap) 
 } else{ NA }

    return(dyad_score)
}



calc_dyad_age_language_shared <- function(single_women, single_men, woman, man, proficiency_threshold = min_speaking_proficiency){
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  woman_speaks = languages[which(single_women[woman, languages] > proficiency_threshold)]
  man_speaks = languages[which(single_men[man, languages] > proficiency_threshold)]
  
  if(woman_speaks %in% man_speaks){
    dyad_score <- abs(age_gap) 
  } else{ NA }
  return(dyad_score)
}

test <- select_marriage_partners(agent_census, calculate_dyad_score = calc_dyad_age_language_max)





#### Check spousal age gap ####
agent_census_with_spouse_age <- agent_census %>%
  left_join(agent_census, by = c("spouse_id" = "agent_id"), suffix = c("", "_spouse"))

# Calculate the age gap
agent_census_with_spouse_age <- agent_census_with_spouse_age %>%
  mutate(age_gap = abs(age - age_spouse))






##### TEST RUN TIME ####
start.time <- Sys.time()

# Your R code here
initial_ages <- generate_age_structure(n = 10000, mortality = CDW15, years = 300)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken



