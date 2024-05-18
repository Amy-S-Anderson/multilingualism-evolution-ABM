



#### Function to assign births to potential mothers ####


# This function needs to:
# 1. Calculate the annual probability of giving birth in this population, as a function of the total fertility rate and the age span of the fertile period. 
# 2. Have female agents of reproductive age interact with that probability and either give birth, or not.
# 3. Return a data frame that indicates which agents give birth this year of model time. 


# I will want to take the output data frame from this function and use it to determine how many new agents to generate and which family ID to assign them. 

########################################################################################



# load library
library(tidyverse)



#### Function: Calculate agent deaths at time t. 

# agent_census = a data frame with a column for agent ID and a column for agent age.
# mortality_regime = a data frame with values for a Siler function of mortality



sow <- function(tfr, agent_census){
  fertile_myrtles <- subset(agent_census, female == 1 & 
                              age >= 15 & age <= 49 &
                              !is.na(spouse_id))
  
  # This equation simulates the yearly likelihood of each female individual of reproductive age giving birth in order to generate an average number of births per year that reflects the assigned total fertility rate (TFR), based on the identified years of reproduction and the starting population size.
  # average annual probability of giving birth
  annual_birth_probability <- (tfr /(49-15))
  fertile_myrtles$baby_dice <- sample(runif(n = 10000, min = 0, max = 1), size = length(fertile_myrtles$agent_id), replace = TRUE)
  fertile_myrtles$baby_time <- if_else(fertile_myrtles$baby_dice < annual_birth_probability, "yes", "no")
  
  new_mothers <- subset(fertile_myrtles, baby_time == "yes") %>%
    select(agent_id, spouse_id)
  
  return(new_mothers)
}





########################################################################################




#### Example use ####

# Need an example data frame that includes matched spouses. 

# total fertility rate: 2 children per woman
# tfr = 2
# test <- sow(tfr, agent_census)


