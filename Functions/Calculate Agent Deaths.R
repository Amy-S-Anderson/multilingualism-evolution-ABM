


#### Function to implement mortality ####


# This function needs to:
# 1. Calculate the age-specific probability of dying, as a function of parameter values from a user-chosen Siler model of population mortality schedules. 
# 2. Have agents interact with that probability and either survive it, or not.
# 3. Return a data frame that indicates which agents will survive to the next year of model time, and which will not. Surviving agents will have one year added to their age status, and non-survivors will not be evaluated in subsequent rounds of model time.

# I will want to take the output data frame from this function and use it as a running census of agent ages and deaths. 

########################################################################################



library(tidyverse)



#### Function: Calculate agent deaths at time t. 

# agent_census = a data frame with a column for agent ID and a column for agent age.
# mortality_regime = a data frame with values for a Siler function of mortality
reap <- function(agent_census, mortality_regime){ 
  
  calculate_age_based_risk <- function(age){mortality_regime$a1 * exp(-mortality_regime$b1 * age) + # infant mortality
      mortality_regime$a2 + # age-independent mortality 
      mortality_regime$a3 * exp(mortality_regime$b3 * age) # senescent mortality
  }
  
  agent_census$age_specific_mortality_risk <- sapply(agent_census$age, FUN = calculate_age_based_risk)
  
  # Now, draw a random number between 0 and 1. (Pick a card, any card...)
  agent_census$death_dice <- sample(runif(n = 10000, min = 0, max = 1), size = length(agent_census$agent_id), replace = TRUE)
  agent_census$time_to_die <- if_else(agent_census$death_dice < agent_census$age_specific_mortality_risk, "yes", "no")
  
  return(agent_census)
}




########################################################################################



#### Example use ####

# Mortality Regime parameters for Siler model. Based on parameter values for Tsimane horticulturalists from Gurven and Kaplan 2007.
Tsimane <- data.frame(a1= 0.221, 
                      b1= 1.193, 
                      a2= 0.009, 
                      a3= 0.000023, 
                      b3= 0.119)

# generate agent IDs
agent_id <- sapply(seq(from = 0, length.out = 1000), FUN = generate_agent_id)

# assign a uniform age structure
age <- sample(0:80, 1000, replace = TRUE)

agents <- data.frame(agent_id, age)

test <- reap(agents, mortality_regime = Tsimane)


ggplot(test, aes(x = age)) +
  geom_bar(aes(fill = as.factor(time_to_die))) + 
  labs(fill = "died") +
  theme_bw()
