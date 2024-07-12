



#### Functions to assign births to potential mothers ####

# A balancing function that results in ***population stationarity*** uses the number of deaths to determine the number of births

# A general function based on average TFR needs to:
# 1. Calculate the annual probability of giving birth in this population, as a function of the total fertility rate and the age span of the fertile period. 
# 2. Have female agents of reproductive age interact with that probability and either give birth, or not.
# 3. Return a data frame that indicates which agents give birth this year of model time. 


# I will want to take the output data frame from this function and use it to determine how many new agents to generate and which family ID to assign them. 

########################################################################################



# load library
library(tidyverse)



#### Function: Calculate which agents become new mothers, maintaining population stationarity

# n_births: an integer, which should match the number of deaths in this year of model time. 
# agent_census = a data frame with columns for agent ID, age, sex, and spouse ID.  

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





#### Function: Calculate which agents become new mothers, allowing for stochasticity around a general population-level TFR ####

# agent_census = a data frame with columns for agent ID, age, sex, and spouse ID.  
# tfr = total fertility rate, the average number of total births per woman.
sow <- function(tfr, agent_census){
  fertile_myrtles <- subset(agent_census, female == 1 & 
                              age >= 15 & age <= 49 &
                              !is.na(spouse_id) &
                              is.na(death_recorded))
  
  # This equation simulates the yearly likelihood of each female individual of reproductive age giving birth in order to generate an average number of births per year that reflects the assigned total fertility rate (TFR), based on the identified years of reproduction and the starting population size.
  # average annual probability of giving birth
  annual_birth_probability <- (tfr /(49-15))
  fertile_myrtles$baby_dice <- sample(runif(n = 10000, min = 0, max = 1), size = length(fertile_myrtles$agent_id), replace = TRUE)
  fertile_myrtles$baby_time <- if_else(fertile_myrtles$baby_dice < annual_birth_probability, "yes", "no")
  
  new_parents <- subset(fertile_myrtles, baby_time == "yes") %>%
    select(agent_id, spouse_id)
  
  return(new_parents)
}






#### Function: Generate agent_census entries for agents born to new mothers ####


# new_mothers = the output of the sow() function above. 
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
      newborns$place_id <- agent_census %>% filter(agent_id %in% new_parents$agent_id) %>% select(place_id)
      # newborns$dad_place_id <- agent_census[which(agent_census$agent_id %in% new_parents$spouse_id),]$place_id
      # newborns <- newborns %>%
      #   rowwise %>%
      #   mutate(
      #     place_id = c_across(sample(c("mom_place_id", "dad_place_id"), 1))) %>%
      #   select(-mom_place_id, -dad_place_id)
      
    }
    agent_census <- rbind(agent_census, newborns)
  }
  return(agent_census)
}




########################################################################################




#### Example use ####

# Need an example data frame that includes matched spouses. 

# total fertility rate: 2 children per woman
# tfr = 2
# test <- sow(tfr, agent_census)

# test <- sow_stationary(agent_census, n_births = 10)
