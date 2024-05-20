



#### Function to assign births to potential mothers ####


# This function needs to:
# 1. Calculate the annual probability of giving birth in this population, as a function of the total fertility rate and the age span of the fertile period. 
# 2. Have female agents of reproductive age interact with that probability and either give birth, or not.
# 3. Return a data frame that indicates which agents give birth this year of model time. 


# I will want to take the output data frame from this function and use it to determine how many new agents to generate and which family ID to assign them. 

########################################################################################



# load library
library(tidyverse)



#### Function: Calculate which agents become new mothers ####

# agent_census = a data frame with columns for agent ID, age, sex, and spouse ID.  
# tfr = total fertility rate, the average number of total births per woman.
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





#### Function: Generate agent_census entries for agents born to new mothers ####


# new_mothers = the output of the sow() function above. 
birth_new_agents <- function(agent_census, new_mothers){
  
  # Create a data frame with a single row of NA values
  newborns <- data.frame(matrix(0, nrow = nrow(new_mothers), ncol = ncol(agent_census)))
  # Set the column names to match those of agent_census
  colnames(newborns) <- colnames(agent_census)
  
  newborns$agent_id <- sapply(seq(from = max(as.numeric(substr(agent_census$agent_id, 4, nchar(agent_census$agent_id)))),
                                  length.out = nrow(new_mothers)), 
                              generate_agent_id)
  newborns$age <- 0
  newborns$female <- sample(c(0,1), size = nrow(new_mothers), replace = T)
  newborns$spouse_id <- NA
  newborns$mother_id <- new_mothers$agent_id
  newborns$father_id <- new_mothers$spouse_id
  newborns$death_recorded <- NA
  
  agent_census <- rbind(agent_census, newborns)
  return(agent_census)
}


########################################################################################




#### Example use ####

# Need an example data frame that includes matched spouses. 

# total fertility rate: 2 children per woman
# tfr = 2
# test <- sow(tfr, agent_census)


