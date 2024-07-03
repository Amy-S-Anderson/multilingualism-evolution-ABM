


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

select_language_of_conversation_at_random <- function(agent_conversation_partners, pop = agent_census){
  
  # Extract the relevant columns once
  language_data <- pop[pop$agent_id %in% agent_conversation_partners, c ("agent_id", languages)] %>%
    pivot_longer(cols = starts_with("Language"), names_to = "can_speak", values_to = "proficiency") %>%
    filter(proficiency > 20) %>%
    group_by(agent_id) %>%
    summarise(spoken = if(n() > 0) sample(can_speak, size = 1))
  
return(language_data$spoken)
}

select_language_of_conversation_at_random(conversants[[1]])
  




##### TEST RUN TIME ####
start.time <- Sys.time()

# Your R code here
initial_ages <- generate_age_structure(n = 10000, mortality = CDW15, years = 300)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken



