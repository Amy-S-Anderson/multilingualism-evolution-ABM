


#### Functions to assign marriage/reproductive partners ####


### Calculate Dyad Scores Functions need to:
# 1. Calculate a compatibility score for each potential pair of partners. Different versions of this function will incorporate
  # partner Age similarity
  # partner language overlap

# 2. Return a matrix of dyad compatibility values (lower = better)

# Calcualte Dyad Scores will only be called inside the select_marriage_partners() function.


### Select Marriage Partners Function needs to:
# 1. Identify the unmarried agents of marriageable age
# 2. Call the the function to calculate the compatibility of potential partners
# 3. Match up the most compatible ones.
# 4. Add new spouse IDs to the agent_census data frame. 

########################################################################################



library(tidyverse)



#### Function: Calculate Dyad Compatibility Scores

# single_women and single_men are subsets of agent_census that are calculated inside the select_marriage_partners() function.
# woman and man are index values for each of these respective subsets.
calc_dyad_age_similarity <- function(single_women, single_men, woman, man){
  
  # calculate age gap for the two individuals in question
  age_gap <- single_men$age[man] - single_women$age[woman]
  
  dyad_score <- abs(age_gap) # dyad compatibility = absolute value of the two agents' difference in age
  return(dyad_score)
}


####### THIS FUNCTION DOESN'T WORK YET -- NEEDS PROFICIENCY SCORES IN THE LANGUAGE COLUMNS IN ORDER TO TEST IT. 
# calc_dyad_age_language1 <- function(single_women, single_men, woman, man, proficiency_threshold = 100){
#   
#   # calculate age gap for the two individuals in question
#   age_gap <- single_men$age[man] - single_women$age[woman]
#   shared_language <- if(colnames(single_men[man, which(language >= proficiency_threshold)] == single_women$agent_id[woman] & single_men[man, language] > x), "yes", "no") ### this definitely doesn't work. come back to this. 
# 
#   
#   dyad_score <- abs(age_gap) + shared_language # dyad compatibility = absolute value of the two agents' difference in age
#   return(dyad_score)
# }
# 

#### Function: Select Marriage Partners

# agent_census = data frame updated in each round t of model time. Contains columns for agent_id, sex, age, language proficiency in however many languages are in the model space, and spouse_id.
# calculate_dyad_score = name of a function from the set of functions above. Choose the function that calculates marriage compatibility based on the marriage rules that you want to implement in this model run. 
select_marriage_partners <- function(agent_census, calculate_dyad_score = calc_dyad_age_similarity){
  
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
  
  
  # identify the most compatible couples
  for (i in 1:min(nrow(single_men), nrow(single_women))) {
    dyad_lowest_score <- min(dyad_scores, na.rm = TRUE)
    dyad_lowest_score_index <- sample(which(dyad_scores == dyad_lowest_score), 1) # indexing from top left to bottom right
    dyad_lowest_score_column <- ceiling(dyad_lowest_score_index / nrow(single_women))
    dyad_lowest_score_row <- dyad_lowest_score_index - nrow(single_women) * (dyad_lowest_score_column - 1)
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
