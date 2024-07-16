


###################### MAKE FILE ############################

# Run this first!

# It loads all the necessary libraries, and loads the function files for the model in the correct order so that functions that depend on objects or functions defined in other files are loaded without errors. 


### Load libraries
library(tidyverse)
library(cowplot)
library(readr)
library(data.table)




### Load bespoke functions
# Note: This is one time that you don't want to aim for efficiency by using an apply function rather than repeating code.
# Run the source() command separately for each function file so that, if something throws an error, the console will give you an error message that tells you exactly which file is glitching when it tries to load. 
source("./Functions//Generate Time0 Agents.R")
source("./Functions//Generate Agent IDs.R")
source("./Functions//Choose languages.R")
source("./Functions//Marriage.R")
source("./Functions//Calculate Agent Births.R")
source("./Functions//Calculate Agent Deaths.R")
source("./Functions//Pair agents for conversations.R")
source("./Functions//Learn by Language Immersion.R")
source("./Functions//Plotting_Functions.R")





### Generate/Load a vector to sample for the age structure created by the pre-determined mortality pattern CDW15
# This will save time while you're developing code for age-structured models. 

### This function generates a set of starting agents from a stable population with a specified mortality hazard and pulls their ages.
#  initial_ages <- generate_age_structure(n = 10000, mortality = CDW15, years = 300) # this line will take a few minutes to run.
#  data.table::fwrite(list(initial_ages), file = "CDW15_age_structure.txt")
initial_ages <- as.numeric(read_lines(file = "CDW15_age_structure.txt")) # read_lines() automatically treats each entry in the .txt file as a character string. Convert to numeric. 





select_language_of_conversation_at_random <- function(agent_conversation_partners, pop = agent_census,
                                                      min_speaking_proficiency = MIN_SPEAKING_PROFICIENCY){
  
  # Extract the relevant columns once
  language_data <- pop[pop$agent_id %in% agent_conversation_partners, c ("agent_id", languages)] %>%
    pivot_longer(cols = starts_with("Language"), names_to = "can_speak", values_to = "proficiency") %>%
    filter(proficiency > min_speaking_proficiency) %>%
    group_by(agent_id) %>%
    summarise(spoken = if(n() > 0) sample(can_speak, size = 1))
  
  return(language_data$spoken)
}


