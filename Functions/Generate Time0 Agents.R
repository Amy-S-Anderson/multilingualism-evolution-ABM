


#### Functions to generate a starting population ####

### Generate population:
# This function needs to:
# 1. Generate an agent_census data frame with all the necessary columns (agent_id, sex, age, parent IDs, spouse ID, language proficiencies, death_recorded)
# 2. run the birth and death functions enough times that the population age distribution produced by these vital rates emerges. 
# 3. Return a data frame of an age-structured population, ready to learn languages from each other. 

# The output data frame is the starting population for the model. 



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

### **This is proving to be quite tricky. Might need help on this one. ** ###

### Choose mortality regime
Tsimane <- data.frame(a1= 0.221,
                      b1= 1.193,
                      a2= 0.009,
                      a3= 0.000023,
                      b3= 0.119)

### Choose fertility rate
fertility = 10


# n = number of starting agents
# fertility = total fertility rate, an integer of average total births per woman 
# mortality = a named data frame of siler parameter values


# n = 1000
# mortality = Tsimane
# generations = 10



push_demography <- function(n, fertility, mortality, years){
  
agent_census <- data.frame(agent_id = sapply(seq(from = 0, length.out = n), FUN = generate_agent_id))

# uniform age structure
agent_census$age <- sample(0:80, n, replace = TRUE)

# alternate assigning male and female state for each agent. 
agent_census$female <- rep(c(0,1), nrow(agent_census)/2)

# create empty variables for record keeping
agent_census$spouse_id <- NA
agent_census$mother_id <- NA
agent_census$father_id <- NA
agent_census$death_recorded <- NA

for(i in seq_len(years)){
  # Agents must marry, because only partnered women become mothers in the sow() function.
  rolling_census <- select_marriage_partners(agent_census, calculate_dyad_score = calc_dyad_age_similarity)
  
  # Assign motherhood
  new_mothers <- sow(tfr = fertility, agent_census = rolling_census)
  
  # Generate new agent data from new mother births.
  rolling_census <- birth_new_agents(rolling_census, new_mothers)
  
  # Record deaths
  rolling_census <- reap(rolling_census, mortality_regime = mortality)
  
  # Update ages of survivors
  rolling_census$age[is.na(rolling_census$death_recorded)] <- rolling_census$age[is.na(rolling_census$death_recorded)] + 1
  rolling_census$year_of_death[is.na(rolling_census$death_recorded)] <- i
  
  # Update the agent_census with the current generation's data
  agent_census <- rolling_census
  
}

# Plot population age structure
p <- ggplot(agent_census[is.na(agent_census$death_recorded),], aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Population Age Structure", x = "Age", y = "Frequency")

return(list(plot = p, final_census = agent_census))
}







test <- push_demography(1000, fertility = 4, mortality = Tsimane, years = 50)



#### There must be a way to create stable populations by calculating the birth rate that balances out the death rate. 





# create columns to language proficiency variables
# agent_languages <- as.data.frame(matrix(0, nrow = nrow(agent_census), ncol = length(languages)))
# names(agent_languages) <- languages
# 
# 
# agent_census <- cbind(agent_census, agent_languages)


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
