


#### Functions to assign marriage/reproductive partners ####


### Calculate Dyad Scores Functions need to:
# 1. Calculate a compatibility score for each potential pair of partners. Different versions of this function will incorporate
  # partner Age similarity
  # partner language overlap
  # partner geographic overlap

# 2. Return a matrix of dyad compatibility values (lower = better)

# Calcualte Dyad Scores will only be called inside the select_marriage_partners() function.


### Select Marriage Partners Function needs to:
# 1. Identify the unmarried agents of marriageable age
# 2. Call the the function to calculate the compatibility of potential partners
# 3. Match up the most compatible ones.
# 4. Add new spouse IDs to the agent_census data frame. 

########################################################################################



library(tidyverse)

#### Marriage Rules: Choose the calculations for determining the marital compatibility between each possible pair of eligible singles.  ####

#### Functions: Calculate Dyad Compatibility Scores

# single_women and single_men are subsets of agent_census that are calculated inside the select_marriage_partners() function.
# woman and man are index values for each of these respective subsets.


# Consider only how similar the two agents are in age. 
calc_dyad_age_similarity <- function(single_women, single_men, woman, man){
  
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  # dyad compatibility = absolute value of the two agents' difference in age, but spousal age gaps of more than 20 years are not allowed. 
  dyad_score <- if(abs(age_gap) < 20){ 
    abs(age_gap)
  } else{NA} 
  return(dyad_score)
}


### Consider age similarity AND require agents to share a language of max proficiency
calc_dyad_age_language_max <- function(single_women, single_men, woman, man, proficiency_threshold = 100){
  
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  woman_max_proficiency = languages[which(single_women[woman, languages] == max(single_women[woman, languages]))]
  man_max_proficiency = languages[which(single_men[man, languages] == max(single_men[man, languages]))]
  
  if(woman_max_proficiency %in% man_max_proficiency){
    dyad_score <- abs(age_gap) 
  } else{ dyad_score <- NA }
  
  return(dyad_score)
}



### Consider age similarity AND require agents to share a spoken language.
# required proficiency threshold to consider a language a shared language defaults to the minimum speaking proficiency, but can be adjusted. 
calc_dyad_age_language_shared <- function(single_women, single_men, woman, man, proficiency_threshold = min_speaking_proficiency){
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  woman_speaks = languages[which(single_women[woman, languages] > proficiency_threshold)]
  man_speaks = languages[which(single_men[man, languages] > proficiency_threshold)]
  
  dyad_score <- if(woman_speaks %in% man_speaks){
    abs(age_gap) 
  } else{ NA }
  return(dyad_score)
}



### Consider age similarity AND constrain agents to marrying only people from their same location. 
calc_dyad_age_and_place <- function(single_women, single_men, woman, man){
  
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  place_compatibility <- single_men$place_id[man] == single_women$place_id[woman]
  # dyad compatibility = sum of the absolute value of the two agents' difference in age 
  dyad_score <- if(place_compatibility){
    abs(age_gap)}
  else{ NA } # if they aren't from the same place, make it impossible for them to get together
  return(dyad_score)
}




#### Function: Select Marriage Partners

# agent_census = data frame updated in each round t of model time. Contains columns for agent_id, sex, age, language proficiency in however many languages are in the model space, and spouse_id.
# calculate_dyad_score = name of a function from the set of functions above. Choose the function that calculates marriage compatibility based on the marriage rules that you want to implement in this model run. 

select_marriage_partners <- function(agent_census, calculate_dyad_score){
    
  # subset unmarried folks
  singles <- agent_census %>%
    filter(is.na(spouse_id),
           age >= 15) 
  single_women <- singles %>% filter(female == 1)
  single_men <- singles %>% filter(female == 0)
  
  
  if(nrow(single_women) > 0 & nrow(single_men) > 0){
    
  # set up empty matrix ready to populate with dyad compatibility scores
  dyad_scores <- matrix(NA, nrow = nrow(single_women), ncol = nrow(single_men))

    for(woman in 1:nrow(single_women)){
      for(man in 1:nrow(single_men)){
        
        dyad_scores[woman,man] <- calculate_dyad_score(single_women, single_men, woman, man)
      }
    }
    rownames(dyad_scores) <- single_women$agent_id
    colnames(dyad_scores) <- single_men$agent_id
  
    
  # Check that anyone is potentially compatible
    for (i in 1:min(nrow(single_men), nrow(single_women))) {
      valid_scores <- dyad_scores[!is.na(dyad_scores)]
      if (length(valid_scores) == 0) {
        break
      }
  # identify the most compatible couples
      dyad_lowest_score <- min(dyad_scores, na.rm = TRUE)
      dyad_lowest_score_index <- which(dyad_scores == dyad_lowest_score, arr.ind = TRUE)
      if (length(dyad_lowest_score_index) == 0) {
        break
      }
      dyad_lowest_score_index <- dyad_lowest_score_index[1, , drop = FALSE]
      dyad_lowest_score_row <- dyad_lowest_score_index[1,1]
      dyad_lowest_score_column <- dyad_lowest_score_index[1,2]
  # Mark these agents as no longer available for pairing  
    dyad_scores[dyad_lowest_score_row, ] <- NA
    dyad_scores[, dyad_lowest_score_column] <- NA
    
  # get them hitched
    paired_woman <- single_women[dyad_lowest_score_row,]$agent_id
    paired_man <- single_men[dyad_lowest_score_column,]$agent_id
    agent_census[which(agent_census$agent_id == paired_man),]$spouse_id <- paired_woman
    agent_census[which(agent_census$agent_id == paired_woman),]$spouse_id <- paired_man
   }
 }
  # agent_census should now have new values in the spouse_id column that correspond to IDs of the newlyweds. 
  return(agent_census)

}



########################################################################################



#### Example use ####
# create data frame of agent traits
agent_census <- data.frame(agent_id = sapply(seq(from = 0, length.out = 1000), FUN = generate_agent_id))

# uniform age structure
agent_census$age <- sample(0:80, 1000, replace = TRUE)

# alternate assigning male and female state for each agent. 
agent_census$female <- rep(c(0,1), nrow(agent_census)/2)

# create empty variable for spouse ID
agent_census$spouse_id <- NA

# create columns to language proficiency variables
agent_languages <- as.data.frame(sapply(languages, function(lang) vector(mode = "list", length = nrow(agent_census))))
names(agent_languages) <- languages

agent_census <- cbind(agent_census, agent_languages)



# simulate 10 years of weddings
years <- 10
singles_count <- list(nrow(agent_census))
for(i in seq(years)){
  
  agent_census <- select_marriage_partners(agent_census, calculate_dyad_score = calc_dyad_age_similarity)
  singles_count[[i + 1]] <- sum(is.na(agent_census$spouse_id))
}
unlist(singles_count)

hist(agent_census[which(is.na(agent_census$spouse_id)),]$age)
# After 5 years, the only unmarried people were under age 20 or over age 60 -- no compatible pairs left, given the population age structure. Once fertility is introduced, this problem should correct itself. 



###############################################################################################

#### Functions for Model 3.0 ####

# Marry them off, at random
marry_random <- function(agents){
  # Identify the eligible singles
  women <- subset(agents, age == 25 & female == 1)
  men <- subset(agents, age == 25 & female == 0)
  
  # Randomly shuffle men to create random pairings
  shuffled_men <- men[sample(nrow(men)), ]
  
  # generate household ID numbers for new couples
  max_household_id <- ifelse(all(is.na(agents$household)), 0, max(agents$household, na.rm = TRUE))
  household_ids <- seq(from = max_household_id + 1, length.out = nrow(women))
  
  # Assign household IDs to the paired couples
  women$household <- household_ids
  shuffled_men$household <- household_ids
  
  # Update the agents data frame with the new household IDs
  agents$household[match(women$agent_id, agents$agent_id)] <- women$household
  agents$household[match(shuffled_men$agent_id, agents$agent_id)] <- shuffled_men$household
  
  return(agents)
  
}





# Match marriage partners according to shared L1, if possible


### This function isn't finished....
marry_shared_L1 <- function(agents, first_language) {
  # Merge the agents data with their first language data
  # Since an agent can have multiple languages, we allow for duplicate agent IDs
  marriageable_agents <- agents %>% 
    subset(age == 25) %>%
    select(agent_id, female, household)
  agents_languages <- merge(marriageable_agents, first_language, by = "agent_id")
  
  # Identify the eligible singles
  women <- subset(agents_languages, female == 1)
  men <- subset(agents_languages, female == 0)
  
  # Generate household ID numbers for new couples
  max_household_id <- ifelse(all(is.na(agents$household)), 0, max(agents$household, na.rm = TRUE))
  
  # Initialize an empty vector for household IDs
  household_ids <- numeric(0)
  
  # List of unique languages from first_language data
  languages <- unique(agents_languages$first_language)
  
  # Keep track of already married agents to prevent multiple marriages
  married_women <- c()
  married_men <- c()
  
  # Loop over each language to match individuals who speak the same first language
  for (lang in languages) {
    # Subset women and men who speak the same first language
    women_lang <- subset(women, first_language == lang & !agent_id %in% married_women)
    men_lang <- subset(men, first_language == lang & !agent_id %in% married_men)
    
    # Check if there are pairs to match
    num_pairs <- min(nrow(women_lang), nrow(men_lang))
    
    if (num_pairs > 0) {
      # Randomly shuffle men to create random pairings
      shuffled_men <- men_lang[sample(nrow(men_lang), num_pairs), ]
      
      # Generate household IDs for the current language group
      new_household_ids <- seq(from = max_household_id + 1, length.out = num_pairs)
      
      # Update household IDs for women and men
      women_lang$household[1:num_pairs] <- new_household_ids
      shuffled_men$household <- new_household_ids
      
      # Append new household IDs
      household_ids <- c(household_ids, new_household_ids)
      
      # Update the agents data frame with the new household IDs
      agents$household[match(women_lang$agent_id[1:num_pairs], agents$agent_id)] <- new_household_ids
      agents$household[match(shuffled_men$agent_id, agents$agent_id)] <- new_household_ids
      
      # Keep track of married individuals to avoid double marriage
      married_women <- c(married_women, women_lang$agent_id[1:num_pairs])
      married_men <- c(married_men, shuffled_men$agent_id)
      
      # Increment the max_household_id for the next group
      max_household_id <- max(new_household_ids)
    }
  }
  
  unmarried <- agents %>%
    filter(is.na(household))
  marry_
  return(agents)
}
