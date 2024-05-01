



############# Assign vertically inherited languages to each agent ##################

# Create a function to assign inherited languages to each agent

## agents = numeric, the number of individuals in the cohort
## languages = character vector of the languages you want to simulate (e.g., c("A", "B"))
inherit_languages <- function(agents, languages) {
  
  agents <- seq(agents) # turn N of agents into a vector of individuals
  mother_tongue <- sample(languages, size = length(agents), replace = TRUE)
  father_tongue <- sample(languages, size = length(agents), replace = TRUE)
  
  # Create a language bank for each agent
  agent_languages <- lapply(languages, function(lang) vector(mode = "list", length = 1))
  names(agent_languages) <- languages
  
  # Inherit languages from parents
  transmit_parent_languages <- function(agent){ for (j in seq(agent_languages)) {
    if (mother_tongue[agent] == names(agent_languages[j]) | father_tongue[agent] == names(agent_languages[j])) {
      agent_languages[[j]][1] <- 1
    }
    else(agent_languages[[j]][1] <- 0)
  }
    return(agent_languages)
  }
  
  agent_languages <- lapply(X = agents, FUN = transmit_parent_languages)
  return(agent_languages)
}
