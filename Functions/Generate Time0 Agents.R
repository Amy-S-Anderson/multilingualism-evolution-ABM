


#### Functions to generate a starting population ####

### Generate population:
# This function needs to:
# 1. Generate an agent_census data frame with all the necessary columns (agent_id, sex, age, parent IDs, spouse ID, language proficiencies, death_recorded)
# 2. run the birth and death functions enough times that the population age distribution produced by these vital rates emerges. 
# 3. Return a data frame of an age-structured population, ready to learn languages from each other. 

# The output data frame is the starting population for the model. 


### Calculate Ages
# This function needs to:
# 1. Assign agent ages at Time0 according to
#  - a reasonable basic distribution,
#  - or, by sampling the distribution of ages in a real-world reference population. 



### Assign language proficiencies in initial population:
# This funciton needs to:
# 1. specify the proportional representation of each language in the population
# 2. assign age-appropriate proficiency values to each speaker
# 3. This first version assumes that everyone is monolingual at Time = 0. 

########################################################################################



#### Function to generate a starting population with a uniform age distribution ####

# This function returns an agent_census data frame with 1:1 sex ratio and a uniform age distribution from 0 to the specified max_age.

# n = desired number of agents
# max_age = oldest age in population of agents.
make_uniform_population <- function(n, max_age){
  agent_census <- data.frame(agent_id = sapply(seq(from = 0, length.out = n), FUN = generate_agent_id))
  
  # uniform age structure
  agent_census$age <- sample(0:max_age, n, replace = TRUE)
  
  # alternate assigning male and female state for each agent. 
  agent_census$female <- rep(c(0,1), nrow(agent_census)/2)
  
  # create empty variables for record keeping
  agent_census$spouse_id <- NA
  agent_census$mother_id <- NA
  agent_census$father_id <- NA
  agent_census$death_recorded <- NA
  agent_census$year <- NA
  
  # create columns to language proficiency variables
  agent_languages <- as.data.frame(matrix(0, nrow = nrow(agent_census), ncol = length(languages)))
  names(agent_languages) <- languages
  
  agent_census <- cbind(agent_census, agent_languages)
  
  return(agent_census)
}


test <- make_uniform_population(500, 90)






#### Function to generate an age-structured starting population ####

calc_age_basic <- function(n_draws, ...){
  rpois(n_draws, 10)
}

test <- calc_age_basic(n_draws = 1000)



make_basic_population <- function(n_draws, max_age){
  agent_census <- data.frame(agent_id = sapply(seq(from = 0, length.out = n), FUN = generate_agent_id))
  
  # uniform age structure
  agent_census$age <- sample(0:max_age, n, replace = TRUE)
  
  # alternate assigning male and female state for each agent. 
  agent_census$female <- rep(c(0,1), nrow(agent_census)/2)
  
  # create empty variables for record keeping
  agent_census$spouse_id <- NA
  agent_census$mother_id <- NA
  agent_census$father_id <- NA
  agent_census$death_recorded <- NA
  agent_census$year <- NA
  
  # create columns to language proficiency variables
  agent_languages <- as.data.frame(matrix(0, nrow = nrow(agent_census), ncol = length(languages)))
  names(agent_languages) <- languages
  
  agent_census <- cbind(agent_census, agent_languages)
  
  return(agent_census)
}


test <- make_uniform_population(500, 90)





#### There must be a way to create stable populations by calculating the birth rate that balances out the death rate. 
#### I think I figured it out ####

# n = number of agents
# mortality = name of designated Siler function variant
# years = length of time to run the simulation in order to get a mortality-determined age structure in a stationary population. 
generate_age_structure <- function(n, mortality, years){
agent_census <- make_uniform_population(n, max_age = 80) # start with a uniform age distribution

# force population size to stay stable by matching fertility to mortality, but allow mortality risk to be a function of age.
 for(t in seq(years)){
 # agent_census$year <- i
  # Record this year's deaths
  agent_census <- reap(agent_census, mortality_regime = CDW15)$agent_census 
  alive <- agent_census[which(is.na(agent_census$death_recorded)),]
  #  Pair up males/females for reproductive partnerships:
  alive <- select_marriage_partners(alive, calculate_dyad_score = calc_dyad_age_similarity)
  
  # - Calculate number of deaths this year based on age structure of population. This will determine the number of births. 
  turnover <- nrow(agent_census) - nrow(alive) 
  # - Generate new births in existing partnerships. Assign traits to newborn agents. 
  new_parents <- sow_stationary(n_births = turnover, alive)
  alive <- birth_new_agents(alive, new_parents)
  
  #- People who survived this round turn 1 year older
  alive$age <- alive$age + 1
  # assign the living back to the data frame that will be exposed to mortality probability at the start of the next loop
  agent_census <- alive
# Repeat all of this living for the next value of time t.
 }

 age <- agent_census$age
 return(age) # the population age distribution should not be uniform anymore -- it should be shaped by the mortality hazard. 
}



test <- generate_age_structure(n = 10000, mortality = CDW15, years = 300)
hist(test)




########################################################################################


#### Function to assign language proficiencies to agents in starting population. ####

# This function returns a population with an even number of monolingual speakers for each language in the population. No family relationships are yet specified, so children old enough to be speaking a language are assigned their language at random. 

assign_starting_proficiency <- function(agent_census){
  
  # function for effect of age on language learning rate -- THIS WILL CHANGE once I have more information from linguists. 
  age_factor <- function(age){
    params <- data.frame(d = 18, a = 0.5, r0 = 9, tc = 0)
    params$r0 * (1 - (1 / (1 + exp(-params$a * (age - params$tc - params$d))))) + 0.5
  }
  
  age = sort(unique(agent_census$age))
  age_rate = sapply(age, FUN = age_factor)
  proficiency_by_age <- data.frame(age,
                                   age_rate,
                                   proficiency = if_else(cumsum(age_rate) <= 100, cumsum(age_rate), 100))
  languages <- agent_census %>%
    select(starts_with("Language")) %>%
    names()
  
  # for each agent
  for(i in 1:nrow(agent_census)){
    language <- sample(languages, size = 1, replace = TRUE) # assign them a language at random
    # ^ The starting frequency of languages will be a changing variable in future model versions. 
    agent_census[i,language] <-proficiency_by_age[which(proficiency_by_age$age == agent_census[i,]$age),]$proficiency
  }
  
  return(agent_census)
}
