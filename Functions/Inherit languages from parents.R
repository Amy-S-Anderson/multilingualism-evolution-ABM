



############# Assign vertically inherited languages to each agent ##################

# Create a function to assign inherited languages to each agent
assign_languages <- function(i) {
  # Create a language bank for each agent
  agent_languages <- lapply(languages, function(lang) vector(mode = "list", length = 1))
  names(agent_languages) <- languages
  
  # Inherit languages from parents
  for (j in 1:length(agent_languages)) {
    if (mother_tongue[i] == names(agent_languages[j]) | father_tongue[i] == names(agent_languages[j])) {
      agent_languages[[j]][1] <- 1
    }
    else(agent_languages[[j]][1] <- 0)
  }
  
  return(agent_languages)
}
