

#### Function to generate unique agent IDs ####

# This function needs to:
# 1. Generate IDs for all the agents present at Time 0
# 2. Generate a new, unique ID for each agent who is born during the model run time. 

# So, the function itself will generate a single ID, but will be built to check the ID value for the most recently assigned agent ID and begin counting from there. 


library(tidyverse)


#### Function: generate_agent_id 
generate_agent_id <- function(start_value) { # the first start_value should be 0. But this means that inside the model, as new agents are born, the numeric portion of the previous agent's ID can be used as the start value to generate the next agent's ID. 
  #agent_number <- start_value + 1
  id <- start_value + 1
  #id <- paste("id", agent_number, sep = "_")
  return(id)
}



### Example use: ###
# generate just one agent ID, the first agent. 
generate_agent_id(start_value = 0)

# generate ID's at Time 0 for a model that is initially populated by 100 agents. 
n = seq(from = 0, length.out = 100)
sapply(n, FUN = generate_agent_id)

